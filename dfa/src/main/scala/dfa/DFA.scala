package dfa // leave this line in the file

case class State(label: String) {}

case class Transition(from: State, to: State, symbol: Char) {
    def findTransition(pos: State, trans: Char) : Option[State] =
        if from == pos && symbol == trans then Some(to)
        else None
}

case class DFA(states: Set[State], transitions: Set[Transition], start: State, accept: Set[State]) {
    def accepts(str: String) = acceptsHelp(str, start)
    def acceptsHelp(str: String, state: State): Boolean =
        if str == "" then
            if accept contains state then true
            else false
        else
            var newState = State("")
            for (s <- states)
            do
                val trans = Transition(state, s, str(0))
                if transitions contains trans then
                    newState = s
            this.acceptsHelp(str.slice(1,str.length), newState)
}
