const std = @import("std");

pub const Error = error{ IllegalCharacter, Incomplete, UnexpectedToken, IllegalToken };
