use id_arena::Id;
use long_ir::{decl::Local, name::Name, ty::Type};
use rustc_hash::FxHashMap as HashMap;

/// A stack of environments.
#[derive(Debug)]
pub struct Envs<'a> {
    pub env_stack: Vec<Env<'a>>,
}

/// Maps names to types.
#[derive(Debug)]
pub struct Env<'a> {
    pub kind: EnvKind<'a>,
    pub map: HashMap<&'a Name, &'a Type<'a>>,
}

/// The kind of an environment.
#[derive(Debug)]
pub enum EnvKind<'a> {
    Global,
    NameSpace(String),
    Block(HashMap<&'a Name, Id<Local<'a>>>),
}

impl<'a> Envs<'a> {
    /// Creates a new `Envs`.
    pub fn new() -> Self {
        Self {
            env_stack: vec![Env {
                kind: EnvKind::Global,
                map: HashMap::default(),
            }],
        }
    }

    /// Pushes a new `NameSpace` environment.
    pub fn push_namespace(&mut self, name: String) {
        self.env_stack.push(Env {
            kind: EnvKind::NameSpace(name),
            map: HashMap::default(),
        })
    }

    /// Pushes a new `Block` environment.
    pub fn push_block(&mut self) {
        self.env_stack.push(Env {
            kind: EnvKind::Block(HashMap::default()),
            map: HashMap::default(),
        });
    }

    /// Pops the current environment.
    pub fn pop(&mut self) -> Option<Env<'a>> {
        self.env_stack.pop()
    }

    /// Adds the given pair of `name` and `ty` to the current environment.
    pub fn add_to_cur_env(&mut self, name: &'a Name, ty: &'a Type<'a>) {
        self.env_stack.last_mut().unwrap().map.insert(name, ty);
    }

    /// Adds the given pair of `name` and `id` to the current environment.
    pub fn add_local_to_cur_env(&mut self, name: &'a Name, id: Id<Local<'a>>) {
        match self.env_stack.last_mut().unwrap() {
            Env {
                kind: EnvKind::Block(map),
                ..
            } => {
                map.insert(name, id);
            }
            _ => panic!(),
        }
    }

    /// Returns the corresponding type of the given `name`.
    pub fn lookup(&self, name: &'a Name) -> Option<&'a Type<'a>> {
        for env in self.env_stack.iter().rev() {
            if let Some(ty) = env.map.get(name) {
                return Some(ty);
            }
        }
        None
    }

    /// Returns the corresponding type of the given `name`.
    pub fn lookup_local(&self, name: &'a Name) -> Option<Id<Local<'a>>> {
        for env in self.env_stack.iter().rev() {
            match env {
                Env {
                    kind: EnvKind::Block(map),
                    ..
                } => {
                    if let Some(&id) = map.get(name) {
                        return Some(id);
                    }
                }
                _ => {}
            }
        }
        None
    }
}

#[test]
fn test() {
    use long_ir::ty::Sign;
    use long_ir::Context;
    let ctx = Context::new();
    let mut envs = Envs::new();
    let gbl1 = ctx.name_arena.alloc(Name::global(vec!["gbl"]));
    let gbl2 = ctx.name_arena.alloc(Name::global(vec!["gbl"]));
    let ty1 = ctx.type_arena.alloc(Type::Int(Sign::Signed));
    let ty2 = ctx.type_arena.alloc(Type::Int(Sign::Signed));
    envs.add_to_cur_env(gbl1, ty1);
    assert_eq!(envs.lookup(gbl2).unwrap(), &*ty2);
}
