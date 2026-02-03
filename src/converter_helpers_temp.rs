
fn format_all_trait_implementations(
    trait_impls: &[&rustdoc_types::Impl],
    crate_data: &Crate,
) -> String {
    let mut output = String::new();
    let mut explicit_impls = Vec::new();
    let mut auto_impls = Vec::new();
    let mut blanket_impls = Vec::new();

    for impl_block in trait_impls {
        if impl_block.is_synthetic {
            auto_impls.push(*impl_block);
        } else if impl_block.blanket_impl.is_some() {
            blanket_impls.push(*impl_block);
        } else {
            explicit_impls.push(*impl_block);
        }
    }

    if !explicit_impls.is_empty() {
        output.push_str(&format_trait_impls_section(
            &explicit_impls,
            crate_data,
            "Trait Implementations",
        ));
    }

    if !auto_impls.is_empty() {
        output.push_str("**Auto Trait Implementations**\n\n");
        let mut auto_traits: Vec<_> = auto_impls
            .iter()
            .filter_map(|i| i.trait_.as_ref())
            .collect();
        auto_traits.sort_by(|a, b| a.path.cmp(&b.path));

        for trait_ref in auto_traits {
            output.push_str(&format!("- `{}`\n", trait_ref.path));
        }
        output.push('\n');
    }

    if !blanket_impls.is_empty() {
        // For blanket implementations, we often just want to see the trait name
        // because the methods are generic. However, showing them using the standard
        // format logic is fine and consistent.
        output.push_str(&format_trait_impls_section(
            &blanket_impls,
            crate_data,
            "Blanket Implementations",
        ));
    }

    output
}

fn format_trait_impls_section(
    impls: &[&rustdoc_types::Impl],
    crate_data: &Crate,
    title: &str,
) -> String {
    let mut output = String::new();
    let mut derives = Vec::new();
    let mut trait_with_methods = Vec::new();

    for impl_block in impls {
        if let Some(trait_ref) = &impl_block.trait_ {
            let methods = format_impl_methods(impl_block, crate_data);
            if methods.is_empty() {
                derives.push(trait_ref.path.as_str());
            } else {
                trait_with_methods.push((trait_ref, methods));
            }
        }
    }

    let public_derives: Vec<_> = derives
        .into_iter()
        .filter(|t| !is_compiler_internal_trait(t))
        .collect();

    // If we only have derives, we might want to put them under "Traits" or similar, 
    // but the caller specifies the section title (e.g. "Trait Implementations").
    // We can group empty-body impls (marker traits) separate from ones with methods.

    if !public_derives.is_empty() {
        // If the title is "Trait Implementations", maybe just calling it "Traits" 
        // for marker traits is slightly confusing vs "Trait Implementations".
        // But let's stick to the existing style where it separates "Traits" (list) 
        // and "Trait Implementations" (with methods).
        
        // However, since we now have section titles passed in, we should probably 
        // structure this carefully.
        
        // Let's dump the marker traits at the top of the section.
        output.push_str(&format!("**{}**\n\n", title));
        
        output.push_str(&format!("The following traits are implemented: {}\n\n", public_derives.join(", ")));
    } else if !trait_with_methods.is_empty() {
         output.push_str(&format!("**{}**\n\n", title));
    }

    if !trait_with_methods.is_empty() {
        for (trait_ref, methods) in trait_with_methods {
            output.push_str(&format!("- **{}**\n", trait_ref.path));
            for line in methods.lines() {
                output.push_str(&format!("  {}\n", line));
            }
        }
        output.push('\n');
    }

    output
}
