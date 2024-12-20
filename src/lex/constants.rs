pub const SPACE: char = ' ';
pub const TAB: char = 0x09 as char;
pub const NEW_LINE: char = 0x0A as char;
pub const NEW_LINE_2: char = 0x0D as char;
pub const CLCR: char = /* 0x0D0A */ 0xDA as char;
pub const ID_CHAR: [char; 23] = [
    '!', '#', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '\\', '^', '_',
    '`', '|', '~', '\'',
];
pub const ESCAPE_CHARACTERS: [char; 6] = ['n', 'r', 't', '\\', '\'', '\"'];
pub const DIGIT: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const HEX_DIGIT: [char; 22] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9',
];
pub const ALL_NUMBER: [char; 13] = [
    '_', '+', '-', 'x', '.', 'e', 'E', 'p', 'P', 'n', 'f', 'a', 'i',
];
