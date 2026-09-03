use anyhow::Context;
use ndc_macros::export_module;
use rand::RngExt;
use rand::distr::Uniform;
use rand::distr::uniform::SampleUniform;
use rand::seq::SliceRandom;
use tap::Tap;

use ndc_vm::value::{SeqValue, Value};

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

#[export_module]
mod inner {
    use itertools::Itertools;
    use ndc_vm::value::AdvancedNumber;

    /// Randomly shuffles the elements of the list in place.
    pub fn shuffle(list: &mut [Value]) {
        list.shuffle(&mut rand::rng());
    }

    /// Returns a copy of the input sequence converted to a list with the elements shuffled in random order.
    ///
    /// Note: this currently does consume iterators
    #[function(return_type = Vec<_>)]
    pub fn shuffled(list: SeqValue) -> anyhow::Result<Value> {
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

    #[function(name = "randf")]
    /// Generate a random Float between 0 (inclusive) and `upper` (exclusive)
    pub fn randf_upper_int(upper: i64) -> anyhow::Result<f64> {
        random_n(0.0, upper as f64)
    }

    #[function(name = "randf")]
    /// Generate a random Float between 0 (inclusive) and `upper` (exclusive)
    pub fn randf_upper_float(upper: f64) -> anyhow::Result<f64> {
        random_n(0.0, upper)
    }

    #[function(name = "randf")]
    /// Generate a random Float between 0 (inclusive) and `upper` (exclusive)
    pub fn randf_upper_number(upper: &AdvancedNumber) -> anyhow::Result<f64> {
        random_n(0.0, upper.try_into()?)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_int_int(lower: i64, upper: i64) -> anyhow::Result<f64> {
        random_n(lower as f64, upper as f64)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_int_float(lower: i64, upper: f64) -> anyhow::Result<f64> {
        random_n(lower as f64, upper)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_float_int(lower: f64, upper: i64) -> anyhow::Result<f64> {
        random_n(lower, upper as f64)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_float_float(lower: f64, upper: f64) -> anyhow::Result<f64> {
        random_n(lower, upper)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_int_number(lower: i64, upper: &AdvancedNumber) -> anyhow::Result<f64> {
        random_n(lower as f64, upper.try_into()?)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_number_int(lower: &AdvancedNumber, upper: i64) -> anyhow::Result<f64> {
        random_n(lower.try_into()?, upper as f64)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_float_number(lower: f64, upper: &AdvancedNumber) -> anyhow::Result<f64> {
        random_n(lower, upper.try_into()?)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_number_float(lower: &AdvancedNumber, upper: f64) -> anyhow::Result<f64> {
        random_n(lower.try_into()?, upper)
    }

    #[function(name = "randf")]
    /// Generate a random Float between `lower` (inclusive) and `upper` (exclusive)
    pub fn randf_number_number(
        lower: &AdvancedNumber,
        upper: &AdvancedNumber,
    ) -> anyhow::Result<f64> {
        random_n(lower.try_into()?, upper.try_into()?)
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
    /// Generate a random Int between 0 (inclusive) and `upper` (exclusive)
    pub fn randi_upper_number(upper: &AdvancedNumber) -> anyhow::Result<i64> {
        random_n(0, upper.try_into()?)
    }

    #[function(name = "randi")]
    /// Generate a random Int between `lower` (inclusive) and `upper` (exclusive)
    pub fn randi_int_int(lower: i64, upper: i64) -> anyhow::Result<i64> {
        random_n(lower, upper)
    }

    #[function(name = "randi")]
    /// Generate a random Int between `lower` (inclusive) and `upper` (exclusive)
    pub fn randi_int_number(lower: i64, upper: &AdvancedNumber) -> anyhow::Result<i64> {
        random_n(lower, upper.try_into()?)
    }

    #[function(name = "randi")]
    /// Generate a random Int between `lower` (inclusive) and `upper` (exclusive)
    pub fn randi_number_int(lower: &AdvancedNumber, upper: i64) -> anyhow::Result<i64> {
        random_n(lower.try_into()?, upper)
    }

    #[function(name = "randi")]
    /// Generate a random Int between `lower` (inclusive) and `upper` (exclusive)
    pub fn randi_number_number(
        lower: &AdvancedNumber,
        upper: &AdvancedNumber,
    ) -> anyhow::Result<i64> {
        random_n(lower.try_into()?, upper.try_into()?)
    }
}
