package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.security.KeyPair;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */

public class ForkJoinSolver extends SequentialSolver {

    static private AtomicBoolean goalFound = new AtomicBoolean();
    static private ConcurrentSkipListSet<Integer> visited = new ConcurrentSkipListSet<>();
    private HashMap<Integer, ForkJoinSolver> solvers;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);
        solvers = new HashMap<>();
    }

    public ForkJoinSolver(int start, Maze maze) {
        this(maze);
        this.start = start;
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
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
     * @return the list of node identifiers from the start node to a
     *         goal node in the maze; <code>null</code> if such a path cannot
     *         be found.
     */
    @Override
    public List<Integer> compute() {
        return parallelSearch(start);
    }

    public HashMap<Integer, ForkJoinSolver> getSolvers() {
        return solvers;
    }

    private List<Integer> parallelSearch(int start) {
        // result to be returned
        List<Integer> pathFromTo = null;

        // visit the start node
        visited.add(start);

        // one player active on the maze at start
        int player = maze.newPlayer(start);
        // start with start node
        frontier.push(start);

        // as long as not all nodes have been processed
        while (!frontier.empty()) {
            // if goal is found by some thread, break the loop.
            if (goalFound.get()) {
                break;
            }

            // get new node to process
            int current = frontier.pop();

            // move player to current node
            maze.move(player, current);

            // if we have reached the goal
            if (maze.hasGoal(current)) {
                goalFound.set(true);
                pathFromTo = pathFromTo(start, current);
                break;
            }

            // go through neighbors of current node
            processNeighbours(current, player);

        }

        return joinSolvers(pathFromTo);
    }

    private void processNeighbours(int current, int player) {
        boolean firstNb = true;
        for (int nb : maze.neighbors(current)) {
            // if nb has been visited, continue, else add to visited
            if (visited.contains(nb)) {
                continue;
            }

            if (firstNb) {
                // current thread should process first neighbour
                visited.add(nb);
                frontier.push(nb);
                predecessor.put(nb, current);
                firstNb = false;
            } else {
                // all other neighbours should be processed by other threads
                createFork(current, nb);
            }
        }
    }

    private void createFork(int current, int nb) {
        // create a new ForkJoinSolver object
        ForkJoinSolver solver = new ForkJoinSolver(nb, maze);

        // save the solver to a hashmap of solvers
        solvers.put(current, solver);

        // fork the solver
        solver.fork();
    }

    private List<Integer> joinSolvers(List<Integer> pathFromTo) {
        for (Map.Entry<Integer, ForkJoinSolver> e : solvers.entrySet()) {
            int current = e.getKey();
            ForkJoinSolver solver = e.getValue();
            List<Integer> path = solver.join();
            if (path != null) {
                pathFromTo = pathFromTo(start, current);
                pathFromTo.addAll(path);
            }
        }

        return pathFromTo;
    }

}