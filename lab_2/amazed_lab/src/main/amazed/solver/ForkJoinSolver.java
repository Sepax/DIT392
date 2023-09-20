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
    extends SequentialSolver
{
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
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
     * @param forkAfter   the number of steps (visited nodes) after
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

        int player = maze.newPlayer(start);
        List<Integer> finalPath = new ArrayList<>();
        finalPath = null;

        frontier.push(start);

        while(!frontier.empty()){

            int current = frontier.pop();

            if (maze.hasGoal(current)){

                hasFoundGoal.set(true);
                maze.move(player, current);
                return pathFromTo(start,current);
            }

            if(!visited.contains(current)){

                int neighborsToCurrent = 0;
                maze.move(player, current);
                visited.add(current);
                
                for (int nb : maze.neighbors(current)){
                    neighborsToCurrent++;
                }
                if (neighborsToCurrent == 1){
                    //continue as usual
                    continue;
                }
                if (neighborsToCurrent > 1){
                    // forloop to create new threads for every new road to explore
                }



            }
        }

        //if not path found return original (= null)
        return finalPath;
    }
}
