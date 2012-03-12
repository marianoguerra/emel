Nonterminals

root parents childs siblings node classes node_filter apply_filter apply_filters node_times attrs attrvals attrval.

Terminals

identifier string number id cls sibling child parent times openattr closeattr
equal filter.

Rootsymbol root.

root -> siblings: '$1'.

siblings -> childs : ['$1'].
siblings -> childs sibling siblings : ['$1'|'$3'].

childs -> parents : '$1'.
childs -> parents child childs : {child, line('$1'), '$1', '$3'}.

parents -> node_times : '$1'.
parents -> node_times parent parents: {parent, line('$1'), '$1', '$3'}.

node_times -> node_filter : '$1'.
node_times -> node_filter times number : {times, line('$1'), '$1', unwrap('$3')}.

node -> identifier : {node, line('$1'), unwrap('$1'), [], []}.
node -> id : {node, line('$1'), 'div', [{id, unwrap('$1')}], []}.
node -> classes : {node, line('$1'), 'div', [{class, unwrap('$1')}], []}.
node -> attrs : {node, line('$1'), 'div', [], unwrap('$1')}.

node -> id classes : {node, line('$1'), 'div', [{id, unwrap('$1')}, {class, unwrap('$2')}], []}.
node -> identifier id : {node, line('$1'), unwrap('$1'), [{id, unwrap('$2')}], []}.
node -> identifier classes : {node, line('$1'), unwrap('$1'), [{class, unwrap('$2')}], []}.
node -> identifier id classes : {node, line('$1'), unwrap('$1'), [{id, unwrap('$2')}, {class, unwrap('$3')}], []}.

node -> attrs id classes : {node, line('$1'), 'div', [{id, unwrap('$2')}, {class, unwrap('$3')}], unwrap('$1')}.
node -> identifier attrs id : {node, line('$1'), unwrap('$1'), [{id, unwrap('$3')}], unwrap('$2')}.
node -> identifier attrs classes : {node, line('$1'), unwrap('$1'), [{class, unwrap('$3')}], unwrap('$2')}.
node -> identifier attrs id classes : {node, line('$1'), unwrap('$1'), [{id, unwrap('$3')}, {class, unwrap('$4')}], unwrap('$2')}.

node_filter -> node : '$1'.
node_filter -> node apply_filters : {filter, line('$1'), '$1', '$2'}.

classes -> cls : {class, line('$1'), [unwrap('$1')]}.
classes -> cls classes : {class, line('$1'), [unwrap('$1')|unwrap('$2')]}.

attrs -> openattr attrvals closeattr : {attrs, line('$1'), '$2'}.

attrval -> identifier: {unwrap('$1'), ""}.
attrval -> identifier equal string : {unwrap('$1'), unwrap('$3')}.

attrvals -> attrval : ['$1'].
attrvals -> attrval attrvals : ['$1'|'$2'].

apply_filter -> filter identifier : unwrap('$2').

apply_filters -> apply_filter : ['$1'].
apply_filters -> apply_filter apply_filters : ['$1'|'$2'].

Erlang code.

unwrap({_,_,V}) -> V.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> element(2, H).
