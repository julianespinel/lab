package executor_service.cached_thread_pool;

import common.LifeCycle;
import common.Stats;
import tasks.TasksFactory;

import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;

public class Experiment {

    public static void main(String[] args) throws InterruptedException {
        Thread.sleep(10000);
        System.out.println("Start");
        int tasksLimit = 10_000;
        List<Runnable> tasks = TasksFactory.createSumAndPrintTasks(tasksLimit);

        ThreadPoolExecutor cachedThreadPool = (ThreadPoolExecutor) Executors.newCachedThreadPool();
        Stats.printThreadPoolStats(cachedThreadPool);
        for (Runnable task : tasks) {
            cachedThreadPool.execute(task);
        }
        // Verify thread pool stats
        int queueSize = cachedThreadPool.getQueue().size();
        while (queueSize != 0) {
            Stats.printThreadPoolStats(cachedThreadPool);
            queueSize = cachedThreadPool.getQueue().size();
            Thread.sleep(300);
        }
        System.out.println("Shutdown");
        LifeCycle.shutDownExecutor(cachedThreadPool);
        System.out.println("End");
        Thread.sleep(10000);
    }
}
