#[derive(Clone, Debug)]
pub struct IrModule {
    pub name: String,
    pub funcs: Vec<IrFunc>,
}

#[derive(Clone, Debug)]
pub struct IrFunc {
    pub name: String,
    pub blocks: Vec<IrBlock>,
}

#[derive(Clone, Debug)]
pub struct IrBlock {
    pub insns: Vec<IrInsn>,
}

#[derive(Clone, Debug)]
pub enum IrInsn {
    // Dummy IR
    Nop,
    Ret,
}

pub fn lower(name: String) -> IrModule {
    IrModule {
        name,
        funcs: vec![IrFunc {
            name: "main".to_string(),
            blocks: vec![IrBlock { insns: vec![IrInsn::Nop, IrInsn::Ret] }],
        }],
    }
}