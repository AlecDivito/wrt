#[derive(Default, Debug, Clone)]
pub struct IndexedVec<V> {
    list: Vec<(Option<String>, V)>,
}

impl<V> IndexedVec<V> {
    pub fn new() -> Self {
        Self { list: vec![] }
    }

    pub fn push_tuple(&mut self, (k, v): (Option<String>, V)) {
        self.list.push((k, v))
    }

    pub fn push(&mut self, k: Option<String>, v: V) {
        self.list.push((k, v))
    }

    pub fn get(&self, key: &str) -> Option<&V> {
        for (k, v) in &self.list {
            if let Some(k) = k {
                if k == key {
                    return Some(v);
                }
            }
        }
        return None;
    }
}
