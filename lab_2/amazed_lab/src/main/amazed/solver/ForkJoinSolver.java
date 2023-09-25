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
    private ArrayList<ForkJoinSolver> solvers;
    private int origin;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);
        solvers = new ArrayList<>();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal. Start node is given by its identifier.
     *
     * @param start the identifier of the start node
     * @param maze  the maze to be searched
     */
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

    public void addOrigin(int origin) {
        this.origin = origin;
    }

    public int getOrigin() {
        return origin;
    }

    private List<Integer> parallelSearch(int start) {
        // path to be returned
        List<Integer> path = null;

        // init player, add start node to frontier.
        int player = maze.newPlayer(start);
        frontier.push(start);

        // as long as not all nodes have been processed
        while (!frontier.empty()) {
            // if goal is found by some thread, break the loop.
            if (goalFound.get()) {
                break;
            }

            // get new node to process
            int current = frontier.pop();

            // move player to current node and mark it as visited.
            visited.add(current);
            maze.move(player, current);

            // if current node is the goal, build path and break the loop.
            if (maze.hasGoal(current)) {
                goalFound.set(true);
                path = pathFromTo(start, current);
                break;
            }

            // if goal is not found, go through neighbors of current node
            processNeighbours(current, player);

        }

        return joinSolvers(path);
    }

    // Process all neighbours of current node. If more than one unvisited neighbour
    // exists, new solvers need to be created.
    private void processNeighbours(int current, int player) {
        boolean firstNeighbour = true;
        for (int nb : maze.neighbors(current)) {
            // if nb has been visited, continue.
            if (visited.contains(nb)) {
                continue;
            }

            // current thread should process first neighbour. All other neighbours should
            // be processed by other threads.
            if (firstNeighbour) {
                frontier.push(nb);
                predecessor.put(nb, current);
                firstNeighbour = false;
            } else {
                createFork(current, nb);
            }
        }
    }

    // Create and fork a new solver thread, and add it to the parent solver's
    // hashmap.
    private void createFork(int current, int nb) {
        if (!visited.contains(nb)) {
            visited.add(nb);
            ForkJoinSolver solver = new ForkJoinSolver(nb, maze);
            solver.addOrigin(current);
            solvers.add(solver);
            solver.fork();
        }
    }

    // Join all child solvers, and add their paths to the parent solver's path. Only
    // paths which are not null are added, which implies that only paths that lead
    // to a goal are added.
    private List<Integer> joinSolvers(List<Integer> path) {
        for (ForkJoinSolver solver : solvers) {
            List<Integer> solverPath = solver.join();
            if (solverPath != null) {
                path = pathFromTo(start, solver.getOrigin());
                path.addAll(solverPath);
            }
        }

        return path;
    }

}