package org.grash

class Symbols {
    private final forwardDefinitions = [:]
    private final definitions = [:]
    
    void addForwardDefinition(sym, val) {
        forwardDefinitions.put sym, val
    }
    
    void addDeclaration(sym) {
        def val = forwardDefinitions.get sym
        if (!val) {
            val = ''
        }
        definitions.put sym, val
        println "Declared '$sym' with value '$val'"
    }
    
    void addDefinition(sym, val) {
        if (!definitions.containsKey(sym)) {
            throw new NotDeclaredException("Symbol $sym defined before declaration")
        }
        definitions.put sym, val
        println "Defined '$sym' with value '$val'"
    }
    
    def getSymbol(sym) {
        if (!definitions.containsKey(sym)) {
            throw new NotDeclaredException("Symbol $sym not found")
        }
        
        return definitions.get(sym)
    }
}

