package org.grash

/**
 * These tokens are used to parse script files.
 */
class Tokens {

    static final WHITE = '[ ]+'
    static final WHITE_ = '[ ]*'
    static final SYM = '[a-zA-Z0-9]+'
    static final REGEXP = '[^:]+'
    static final VAL = '.+'
    static final PATH = '[^:]+'
    static final ACTION = /(prm|seq|alt|set)\(.+\)/
    static final COMMENT = '#.*'
	
}

