package org.grash

/**
 * A symbol table, combined with the actual value mappings
 * of all symbols.
 */
class Symbols {
    private final forwardDefinitions = [:]
    private final definitions = [:]
    
    /**
     * Forward definitions can be made by passing parameter definitions
     * as command line parameters. Keep these around in order to be able
     * to apply them later when parameters are actually declared.
     */
    void addForwardDefinition(sym, val) {
        forwardDefinitions.put sym, val
    }
    
    /**
     * Add a symbol to our table. The value is either initialized to
     * the empty string or its forward definition if it exists.
     */
    void addDeclaration(sym) {
        def val = forwardDefinitions.get sym
        if (!val) {
            val = ''
        }
        definitions.put sym, val
        println "Declared '$sym' with value '$val'"
    }
    
    /**
     * Maps a value to a symbol (the symbol must exist).
     */
    void addDefinition(sym, val) {
        if (!definitions.containsKey(sym)) {
            throw new NotDeclaredException("Symbol $sym defined before declaration")
        }
        definitions.put sym, val
        println "Defined '$sym' with value '$val'"
    }
    
    /**
     * Retrieves the value of a symbol (the symbol must exist).
     */
    def getSymbol(sym) {
        if (!definitions.containsKey(sym)) {
            throw new NotDeclaredException("Symbol $sym not found")
        }
        
        return definitions.get(sym)
    }
}

