const std = @import("std");

pub const Error = error{
// syntax errors
    IllegalCharacter,
    Incomplete,
    UnexpectedToken,
    IllegalToken,
    MissingLabel,
    UnexpectedLabel,
    // semantical errors
    DuplicateConstant,
};
