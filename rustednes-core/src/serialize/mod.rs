use super::nes;
use super::nes::Nes;

use serde_derive::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub enum VersionedState {
    Version1(nes::State),
}

pub fn get_state(nes: &Nes) -> VersionedState {
    VersionedState::Version1(nes.get_state())
}

pub fn apply_state(nes: &mut Nes, state: VersionedState) {
    use self::VersionedState::*;
    match state {
        Version1(ref state) => nes.apply_state(state),
    }
}
