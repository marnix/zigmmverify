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
    // proof errors
    HypothesisMismatch,
    ResultMismatch,
    DVRMissing,
    // compressed proof errors
    NumberIncomplete,
    NumberZEarly,
    NumberTooLarge,
};
