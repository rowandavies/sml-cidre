datatype node = Node of {body:phrase, parent:node, typ:typ} ref
              | NoNode
     and phrase = Var of string | Abs of node * node 
                | App of node * node | Let of node * node
                | Binder of string
                | Definition of node * node
     and typ = TypCon | NoTyp

(*[ datasort TypCon = TypCon and NoTyp = NoTyp ]*)

(*[
datasort expnode = Node of {body:>expphrase, parent:>expnodeUdefnodeUnonode, typ:>TypCon} ref
     and bindernode = Node of {body:>binderphrase, parent:>expnodeUdefnodeUnonode, typ:>NoTyp} ref
     and defnode = Node of {body:>defphrase, parent:>expnodeUnonode, typ:>TypCon} ref
     and expphrase = Var of string | Abs of bindernode * expnode 
                   | App of expnode * expnode | Let of defnode * expnode
     and binderphrase = Binder of string
     and defphrase = Definition of bindernode * expnode
     and expnodeUdefnodeUnonode = 
           Node of {body:>expphrase, parent:>expnodeUdefnodeUnonode, typ:>TypCon} ref
         | Node of {body:>defphrase, parent:>expnodeUnonode, typ:>TypCon} ref
         | NoNode
     and expnodeUnonode = 
           Node of {body:>expphrase, parent:>expnodeUdefnodeUnonode, typ:>TypCon} ref
         | NoNode
]*)