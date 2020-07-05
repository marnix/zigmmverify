usingnamespace @import("globals.zig");

pub const Error = error{
// syntax errors
    IllegalCharacter,
    Incomplete,
    UnexpectedToken,
    IllegalToken,
    MissingLabel,
    UnexpectedLabel,
    // semantical errors
    Duplicate,
};
