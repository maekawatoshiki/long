use long_ir::name::Name;

pub fn mangle_name(name: &Name) -> String {
    // TODO: Names not mangled yet.
    format!("{}", name).trim_start_matches("::").to_owned()
}
