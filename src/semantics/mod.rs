use crate::{ast::Module, error::Result};

#[derive(Clone, Debug)]
pub struct CheckedModule {
    pub module: Module,
    // senere: symbol table, types, resolved ids, etc.
}

pub fn check(module: Module) -> Result<CheckedModule> {
    // Dummy: ingen checks endnu
    Ok(CheckedModule { module })
}