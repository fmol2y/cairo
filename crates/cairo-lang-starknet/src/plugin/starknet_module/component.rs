use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::attribute::structured::{AttributeArg, AttributeArgVariant};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use indoc::indoc;

use super::generation_data::{ComponentGenerationData, StarknetModuleCommonGenerationData};
use super::StarknetModuleKind;
use crate::plugin::consts::{INCLUDABLE_AS_ATTR, STORAGE_STRUCT_NAME};
use crate::plugin::storage::handle_storage_struct;

/// Accumulated data specific for component generation.
#[derive(Default)]
pub struct ComponentSpecificGenerationData {
    generated_impls: Vec<RewriteNode>,
}
impl ComponentSpecificGenerationData {
    pub fn to_rewrite_node(self) -> RewriteNode {
        RewriteNode::new_modified(self.generated_impls)
    }
}

pub(super) fn generate_component_specific_code(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    common_data: StarknetModuleCommonGenerationData,
    body: &ast::ModuleBody,
) -> RewriteNode {
    let mut generation_data = ComponentGenerationData { common: common_data, ..Default::default() };
    for item in body.items(db).elements(db) {
        handle_component_item(db, diagnostics, &item, &mut generation_data);
    }
    generation_data.to_rewrite_node()
}

fn handle_component_item(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    data: &mut ComponentGenerationData,
) {
    match &item {
        ast::Item::Impl(item_impl) => {
            let TODOyg = handle_component_impl(db, diagnostics, item_impl, data);
        }
        ast::Item::Struct(item_struct) if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME => {
            handle_storage_struct(
                db,
                diagnostics,
                item_struct.clone(),
                StarknetModuleKind::Component,
                &mut data.common,
            );
        }
        _ => {}
    }
}

// TODO(yg): doc
fn get_includable_as_attr_value(db: &dyn SyntaxGroup, attr: &ast::Attribute) -> Option<ast::Expr> {
    let ast::OptionArgListParenthesized::ArgListParenthesized(attribute_args) = attr.arguments(db)
    else {
        return None;
    };

    let [arg] = &attribute_args.args(db).elements(db)[..] else {
        return None;
    };
    let AttributeArgVariant::Unnamed { value: attr_arg_value, .. } =
        AttributeArg::from_ast(arg.clone(), db).variant
    else {
        return None;
    };

    Some(attr_arg_value)
}

// TODO(yg): doc
fn get_includable_as_generic_params(
    db: &dyn SyntaxGroup,
    item_impl: &ast::ItemImpl,
) -> Option<ast::GenericParamList> {
    let generic_params = item_impl.generic_params(db);
    let ast::OptionWrappedGenericParamList::WrappedGenericParamList(params) = generic_params else {
        return None;
    };
    let generic_params_node = params.generic_params(db);
    let generic_param_elements = generic_params_node.elements(db);
    if !generic_param_elements
        .first()
        .and_then(|param| try_extract_matches!(param, ast::GenericParam::Type))
        .map_or(false, |param| param.name(db).text(db) == "TContractState")
    {
        return None;
    }
    // TODO(yg): verify second generic param?

    Some(generic_params_node)
}

/// Handles an impl inside a component module.
fn handle_component_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item_impl: &ast::ItemImpl,
    data: &mut ComponentGenerationData,
) -> Result<(), PluginDiagnostic> {
    let Some(attr) = item_impl.find_attr(db, INCLUDABLE_AS_ATTR) else {
        return Ok(());
    };

    // TODO(yg): extract all validations to a function.
    let Some(attr_arg_value) = get_includable_as_attr_value(db, &attr) else {
        return Err(PluginDiagnostic {
            message: "`includable_as` attribute must have a single unnamed argument for the \
                      generated impl name, e.g.: #[includable_as(MyImpl)]"
                .into(),
            stable_ptr: attr.stable_ptr().untyped(),
        });
    };

    let Some(generic_params_node) = get_includable_as_generic_params(db, item_impl) else {
        return Err(PluginDiagnostic {
            message: "First generic parameter of an impl with #[includable_as] should be \
                      `TContractState`"
                .into(),
            stable_ptr: attr.stable_ptr().untyped(),
        });
    };

    let ast::MaybeImplBody::Some(impl_body) = item_impl.body(db) else {
        return Err(PluginDiagnostic {
            message: "`includable_as` attribute is not supported for empty impls".into(),
            stable_ptr: attr.stable_ptr().untyped(),
        });
    };

    let maybe_comma = if generic_params_node.has_tail(db) {
        RewriteNode::Text(",".to_string())
    } else {
        RewriteNode::empty()
    };

    let impl_name = RewriteNode::new_trimmed(item_impl.name(db).as_syntax_node());
    let generated_trait_name = RewriteNode::interpolate_patched(
        "$impl_name$Trait",
        [("impl_name".to_string(), impl_name)].into(),
    );

    let mut trait_functions = vec![];
    let mut impl_functions = vec![];
    for item in impl_body.items(db).elements(db) {
        let Some((trait_function, impl_function)) =
            handle_component_impl_function(db, diagnostics, item)
        else {
            continue;
        };
        trait_functions.push(RewriteNode::Text("\n    ".to_string()));
        trait_functions.push(trait_function);
        impl_functions.push(RewriteNode::Text("\n    ".to_string()));
        impl_functions.push(impl_function);
    }

    let generated_impl_node = RewriteNode::interpolate_patched(
        indoc! {"
        trait $generated_trait_name$<TContractState> {$trait_functions$
        }

        impl $generated_impl_name$<$generic_params$$maybe_comma$ impl TContractStatePanicDestruct: \
         PanicDestruct<TContractState>> of $generated_trait_name$<TContractState> {$impl_functions$
        }"},
        [
            ("generated_trait_name".to_string(), generated_trait_name),
            ("trait_functions".to_string(), RewriteNode::new_modified(trait_functions)),
            (
                "generated_impl_name".to_string(),
                RewriteNode::Copied(attr_arg_value.as_syntax_node()),
            ),
            (
                "generic_params".to_string(),
                RewriteNode::Copied(generic_params_node.as_syntax_node()),
            ),
            ("maybe_comma".to_string(), maybe_comma),
            ("impl_functions".to_string(), RewriteNode::new_modified(impl_functions)),
        ]
        .into(),
    );

    data.specific.generated_impls.push(generated_impl_node);

    Ok(())
}

fn handle_component_impl_function(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: ast::ImplItem,
) -> Option<(RewriteNode, RewriteNode)> {
    let ast::ImplItem::Function(item_function) = item else {
        return None;
    };

    let declaration = item_function.declaration(db);
    let signature = declaration.signature(db);
    let params = signature.parameters(db).elements(db);

    let function_name = RewriteNode::new_trimmed(declaration.name(db).as_syntax_node());
    let Some((first_param, rest_params)) = params.split_first() else {
        // TODO(yg): diagnostic?
        return None;
    };
    // TODO(yg): do this better? (allow whitespaces...)
    let (self_param, get_component_call) = match first_param.as_syntax_node().get_text(db).as_str()
    {
        "self: @ComponentState<TContractState>" => {
            ("self: @TContractState", "let component = self.get_component();")
        }
        "ref self: ComponentState<TContractState>" => {
            ("ref self: TContractState", "let mut component = self.get_component_mut();")
        }
        _ => {
            // TODO(yg): diagnostic?
            return None;
        }
    };
    let rest_params_node = RewriteNode::new_modified(
        rest_params
            .into_iter()
            .flat_map(|p| {
                vec![RewriteNode::Text(", ".to_string()), RewriteNode::Copied(p.as_syntax_node())]
            })
            .collect(),
    );
    let args_node = RewriteNode::new_modified(
        rest_params
            .into_iter()
            .flat_map(|p| {
                vec![
                    RewriteNode::Copied(p.name(db).as_syntax_node()),
                    RewriteNode::Text(", ".to_string()),
                ]
            })
            .collect(),
    );
    let ret_ty = match signature.ret_ty(db) {
        ast::OptionReturnTypeClause::Empty(_) => RewriteNode::empty(),
        ast::OptionReturnTypeClause::ReturnTypeClause(x) => RewriteNode::interpolate_patched(
            " $ret_ty$",
            [("ret_ty".to_string(), RewriteNode::new_trimmed(x.as_syntax_node()))].into(),
        ),
    };

    let generated_function_sig = RewriteNode::interpolate_patched(
        format!("fn $function_name$({self_param}$rest_params_node$)$ret_ty$").as_str(),
        [
            ("function_name".to_string(), function_name.clone()),
            ("rest_params_node".to_string(), rest_params_node),
            ("ret_ty".to_string(), ret_ty),
        ]
        .into(),
    );

    let trait_function = RewriteNode::interpolate_patched(
        "$generated_function_sig$;",
        [("generated_function_sig".to_string(), generated_function_sig.clone())].into(),
    );

    let impl_function = RewriteNode::interpolate_patched(
        format!(
            "$generated_function_sig$ {{
        {get_component_call}
        component.$function_name$($args_node$)
    }}"
        )
        .as_str(),
        [
            ("generated_function_sig".to_string(), generated_function_sig),
            ("function_name".to_string(), function_name),
            ("args_node".to_string(), args_node),
        ]
        .into(),
    );

    Some((trait_function, impl_function))
}
