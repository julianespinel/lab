package executor_service.fixed_thread_pool;

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

        int threadsLimit = 16;
        ThreadPoolExecutor fixedThreadPool = (ThreadPoolExecutor) Executors.newFixedThreadPool(threadsLimit);
        Stats.printThreadPoolStats(fixedThreadPool);
        for (Runnable task : tasks) {
            fixedThreadPool.execute(task);
        }
        // Verify thread pool stats
        int queueSize = fixedThreadPool.getQueue().size();
        while (queueSize != 0) {
            Stats.printThreadPoolStats(fixedThreadPool);
            queueSize = fixedThreadPool.getQueue().size();
            Thread.sleep(300);
        }
        System.out.println("Shutdown");
        LifeCycle.shutDownExecutor(fixedThreadPool);
        System.out.println("End");
        Thread.sleep(10000);
    }
}
