use anyhow::Context;
use ndc_macros::export_module;
use rand::RngExt;
use rand::distr::Uniform;
use rand::distr::uniform::SampleUniform;
use rand::seq::SliceRandom;
use tap::Tap;

use ndc_vm::value::Value;

pub fn random_n<N: SampleUniform + std::fmt::Display + Copy>(
    lower: N,
    upper: N,
) -> anyhow::Result<N> {
    let mut rng = rand::rng();
    let side: Uniform<N> = Uniform::new(lower, upper).with_context(|| {
        format!("Lower bound ({lower}) cannot be greater than upper bound ({upper}).")
    })?;
    Ok(rng.sample(side))
}

/// A bound of `randf`, any real number, as a Float.
fn float_bound(value: &Value) -> anyhow::Result<f64> {
    value
        .numeric_ref()
        .and_then(|number| number.to_f64())
        .ok_or_else(|| anyhow::anyhow!("{value} can't be a bound of randf"))
}

#[export_module]
mod inner {
    use itertools::Itertools;

    /// Randomly shuffles the elements of the list in place.
    #[function(signature = "<T>([T])")]
    pub fn shuffle(list: &mut [Value]) {
        list.shuffle(&mut rand::rng());
    }

    /// Returns a copy of the input sequence converted to a list with the elements shuffled in random order.
    ///
    /// Note: this currently does consume iterators
    #[function(signature = "<T>(Iterable<T>) -> [T]")]
    pub fn shuffled(list: Value) -> anyhow::Result<Value> {
        Ok(Value::list(
            list.try_into_iter()
                .ok_or_else(|| anyhow::anyhow!("shuffled requires a sequence"))?
                .collect_vec()
                .tap_mut(|v| v.shuffle(&mut rand::rng())),
        ))
    }

    #[function(name = "randf")]
    /// Generate a random Float between 0 (inclusive) and 1 (exclusive)
    pub fn randf_0() -> anyhow::Result<f64> {
        random_n(0.0, 1.0)
    }

    /// Generate a random Float between 0 (inclusive) and `upper` (exclusive)
    #[function(name = "randf", signature = "(Int) -> Float")]
    #[function(name = "randf", signature = "(Float) -> Float")]
    #[function(name = "randf", signature = "(Number) -> Float")]
    pub fn randf_upper(upper: Value) -> anyhow::Result<f64> {
        random_n(0.0, float_bound(&upper)?)
    }

    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    #[function(signature = "(Int, Int) -> Float")]
    #[function(signature = "(Int, Float) -> Float")]
    #[function(signature = "(Int, Number) -> Float")]
    #[function(signature = "(Float, Int) -> Float")]
    #[function(signature = "(Float, Float) -> Float")]
    #[function(signature = "(Float, Number) -> Float")]
    #[function(signature = "(Number, Int) -> Float")]
    #[function(signature = "(Number, Float) -> Float")]
    #[function(signature = "(Number, Number) -> Float")]
    pub fn randf(lower: Value, upper: Value) -> anyhow::Result<f64> {
        random_n(float_bound(&lower)?, float_bound(&upper)?)
    }

    #[function(name = "randi")]
    /// Generate a random Int between 0 (inclusive) and the maximum Int value (exclusive)
    pub fn randi_0() -> anyhow::Result<i64> {
        random_n(0, i64::MAX)
    }

    #[function(name = "randi")]
    /// Generate a random Int between 0 (inclusive) and `upper` (exclusive)
    pub fn randi_upper_int(upper: i64) -> anyhow::Result<i64> {
        random_n(0, upper)
    }

    #[function(name = "randi")]
    /// Generate a random Int between `lower` (inclusive) and `upper` (exclusive)
    pub fn randi_int_int(lower: i64, upper: i64) -> anyhow::Result<i64> {
        random_n(lower, upper)
    }
}
