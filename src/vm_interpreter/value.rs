use std::fmt::Debug;

#[derive(Clone)]
pub struct Value {
    value: f64,
}
impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self { value }
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl Value {
    pub fn cast_float(&self) -> f64 {
        self.value
    }
}
