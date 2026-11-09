#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Load {
    Load,
    Clear,
    DontCare,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Store {
    Store,
    Discard,
    Resolve,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Rect {
    pub x: i64,
    pub y: i64,
    pub width: i64,
    pub height: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cull {
    CullNone,
    CullFront,
    CullBack,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison {
    Never,
    Less,
    Equal,
    LessEqual,
    Greater,
    NotEqual,
    GreaterEqual,
    Always,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    PointList,
    LineList,
    TriangleList,
    TriangleStrip,
    LineStrip,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexType {
    Uint16,
    Uint32,
}
