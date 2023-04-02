package common;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class LifeCycle {

    /**
     * Send shutdown request, but wait up to 1 minute for all tasks to be completed.
     * After one minute if not all tasks have been completed, the executor will shut down
     * and the remaining tasks will not be executed.
     *
     * @param executorService An object implementing the ExecutorService interface.
     * @throws InterruptedException
     */
    public static void shutDownExecutor(ExecutorService executorService) throws InterruptedException {
        //
        executorService.shutdown();
        if (!executorService.awaitTermination(1, TimeUnit.MINUTES)) {
            List<Runnable> tasksNotExecuted = executorService.shutdownNow();
            System.out.println("Number of not executed tasks: " + tasksNotExecuted.size());
        }
    }
}
