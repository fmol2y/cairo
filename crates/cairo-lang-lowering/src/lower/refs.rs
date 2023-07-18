use cairo_lang_defs::ids::MemberId;
use cairo_lang_semantic as semantic;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::usage::MemberPath;
use crate::ids::LocationId;
use crate::{VarUsage, VariableId};

/// Maps member paths ([MemberPath]) to lowered variable ids.
#[derive(Clone, Default, Debug)]
pub struct SemanticLoweringMapping {
    scattered: OrderedHashMap<MemberPath, Value>,
}
impl SemanticLoweringMapping {
    pub fn contains_var(&mut self, var: &semantic::VarId) -> bool {
        self.scattered.contains_key(&MemberPath::Var(*var))
    }

    pub fn get<TContext: StructRecomposer>(
        &mut self,
        mut ctx: TContext,
        path: &MemberPath,
    ) -> Option<VariableId> {
        let value = self.break_into_value(&mut ctx, path)?;
        Self::assemble_value(&mut ctx, value).map(|usage| usage.var_id)
    }

    pub fn introduce(&mut self, path: MemberPath, var: VariableId, location: LocationId) {
        self.scattered.insert(path, Value::Var(var, location));
    }

    pub fn update<TContext: StructRecomposer>(
        &mut self,
        mut ctx: TContext,
        path: &MemberPath,
        var: VariableId,
        location: LocationId,
    ) -> Option<()> {
        let value = self.break_into_value(&mut ctx, path)?;
        *value = Value::Var(var, location);
        Some(())
    }

    fn assemble_value<TContext: StructRecomposer>(
        ctx: &mut TContext,
        value: &mut Value,
    ) -> Option<VarUsage> {
        Some(match value {
            Value::Var(var, location) => VarUsage { var_id: *var, location: *location },
            Value::Scattered(scattered) => {
                let location = scattered.location;
                let members = scattered
                    .members
                    .iter_mut()
                    .map(|(_, value)| Self::assemble_value(ctx, value))
                    .collect::<Option<_>>()?;
                let var = ctx.reconstruct(scattered.concrete_struct_id, members);
                *value = Value::Var(var, location);
                VarUsage { var_id: var, location }
            }
        })
    }

    fn break_into_value<TContext: StructRecomposer>(
        &mut self,
        ctx: &mut TContext,
        path: &MemberPath,
    ) -> Option<&mut Value> {
        if self.scattered.contains_key(path) {
            return self.scattered.get_mut(path);
        }

        let MemberPath::Member { parent, member_id, concrete_struct_id, .. } = path else {
            return None;
        };

        let parent_value = self.break_into_value(ctx, parent)?;
        match parent_value {
            Value::Var(var, location) => {
                let members = ctx.deconstruct(
                    *concrete_struct_id,
                    VarUsage { var_id: *var, location: *location },
                );
                let members = OrderedHashMap::from_iter(
                    members
                        .into_iter()
                        .map(|(member_id, var)| (member_id, Value::Var(var, *location))),
                );
                let scattered = Scattered {
                    concrete_struct_id: *concrete_struct_id,
                    members,
                    location: *location,
                };
                *parent_value = Value::Scattered(Box::new(scattered));

                extract_matches!(parent_value, Value::Scattered).members.get_mut(member_id)
            }
            Value::Scattered(scattered) => scattered.members.get_mut(member_id),
        }
    }
}

/// A trait for deconstructing and constructing structs.
pub trait StructRecomposer {
    fn deconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        value: VarUsage,
    ) -> OrderedHashMap<MemberId, VariableId>;
    fn reconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        members: Vec<VarUsage>,
    ) -> VariableId;
    fn var_ty(&self, var: VariableId) -> semantic::TypeId;
}

/// An intermediate value for a member path.
#[derive(Clone, Debug)]
enum Value {
    /// The value of member path is stored in a lowered variable.
    Var(VariableId, LocationId),
    /// The value of the member path is not stored. It should be reconstructed from the member
    /// values.
    Scattered(Box<Scattered>),
}

/// A value for an non-stored member path. Recursively holds the [Value] for the members.
#[derive(Clone, Debug)]
struct Scattered {
    concrete_struct_id: semantic::ConcreteStructId,
    members: OrderedHashMap<MemberId, Value>,
    location: LocationId,
}
