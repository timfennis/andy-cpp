use anyhow::Context;
use ndc_macros::export_module;
use rand::Rng;
use rand::distr::Uniform;
use rand::distr::uniform::SampleUniform;
use rand::seq::SliceRandom;

pub fn random_n<N: SampleUniform + std::fmt::Display + Copy>(
    lower: N,
    upper: N,
) -> anyhow::Result<N> {
    let mut rng = rand::rng();
    let side: Uniform<N> = Uniform::new(lower, upper).context(format!(
        "Lower bound ({lower}) cannot be greater than upper bound ({upper})."
    ))?;
    Ok(rng.sample(side))
}

#[export_module]
mod inner {
    use crate::interpreter::num::Number;
    use crate::interpreter::value::Value;

    pub fn shuffle(list: &mut [Value]) {
        list.shuffle(&mut rand::rng());
    }

    #[function(name = "randf")]
    /// Generate a random number between 0 (inclusive) and 1 (exclusive)
    pub fn randf_0() -> anyhow::Result<f64> {
        random_n(0.0, 1.0)
    }

    #[function(name = "randf")]
    /// Generate a random number between 0 (inclusive) and `upper` (exclusive)
    pub fn randf_1(upper: &Number) -> anyhow::Result<f64> {
        random_n(0.0, upper.try_into()?)
    }

    #[function(name = "randf")]
    /// Generate a random number between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_2(lower: &Number, upper: &Number) -> anyhow::Result<f64> {
        random_n(lower.try_into()?, upper.try_into()?)
    }

    #[function(name = "randi")]
    /// Generate a random number between 0 (inclusive) and 1 (exclusive)
    pub fn randi_0() -> anyhow::Result<i64> {
        random_n(0, i64::MAX)
    }

    #[function(name = "randi")]
    /// Generate a random number between 0 (inclusive) and `upper` (exclusive)
    pub fn randi_1(upper: &Number) -> anyhow::Result<i64> {
        random_n(0, upper.try_into()?)
    }

    #[function(name = "randi")]
    /// Generate a random number between `lower` (inclusive) and `upper` (exclusive)
    pub fn randi_2(lower: &Number, upper: &Number) -> anyhow::Result<i64> {
        random_n(lower.try_into()?, upper.try_into()?)
    }
}
