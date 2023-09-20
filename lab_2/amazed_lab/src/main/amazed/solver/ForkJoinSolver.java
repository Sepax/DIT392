package amazed.solver;

import amazed.maze.Maze;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver{
        
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */

     // class to create new "threads" when forking 
protected class FThread{
            final int forkStart;
            final ForkJoinSolver fthread;

            FThread(int forkStart , ForkJoinSolver fthread){
                this.forkStart = forkStart;
                this.fthread = fthread;
            }
        }

    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAFter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    AtomicBoolean hasFoundGoal = new AtomicBoolean(false);


    @Override
    public List<Integer> compute()
    {
        return parallelSearch(start);
    }

    private List<Integer> parallelSearch(int start){
        // one player active on the maze at start
        int player = maze.newPlayer(start);
        // start with start node
        frontier.push(start);
        // as long as not all nodes have been processed

        while (!frontier.empty()) {
            // get the new node to process
            int current = frontier.pop();
            // if current node has a goal
            if (maze.hasGoal(current)) {
                //global atom boolean to tell other threads goal has been found
                hasFoundGoal.set(true);
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                return pathFromTo(start, current);
            }
            // if current node has not been visited yet
            if (!visited.contains(current)) {
                // move player to current node
                maze.move(player, current);
                // mark node as visited
                visited.add(current);
                // for every node nb adjacent to current
                int nbCount = 0;
                for (int nb: maze.neighbors(current)) {
                    // add nb to the nodes to be processed
                    frontier.push(nb);
                    // if nb has not been already visited,
                    // nb can be reached from current (i.e., current is nb's predecessor)
                    if (!visited.contains(nb))
                        predecessor.put(nb, current);
                        nbCount++;
                        if (nbCount == 1) {
                            // this is the next "current" for this thread
                        } else  {
                            new FThread(nb, new ForkJoinSolver(maze));
                            
                        } 
                    }
            }

        }
        // all nodes explored, no goal found
        return null;
    }

}


