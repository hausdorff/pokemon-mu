ROADMAP:
* Find a way to unify edges, because we need it to both avoid duplicate self-loops and to unify regexes.

Think through regex unification system:
* Get a point.
* Get one of the edges going in, and loop through the outgoing edges.
* For each of these, check:
  * if the outgoing edge is going back to the original point; if so, we should cat the ingoing edge with the self-loops on that node and the outgoing edge, and star it all. Then put that on the in-edge node.
  * if the outgoing edge is going to a different node, cat the in edge, the self-loops, and the out-edge. Put that on the in-edge node.