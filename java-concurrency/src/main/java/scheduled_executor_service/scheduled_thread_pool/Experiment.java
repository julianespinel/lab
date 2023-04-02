package scheduled_executor_service.scheduled_thread_pool;

import common.LifeCycle;
import common.Stats;
import tasks.TasksFactory;

import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class Experiment {

    public static void main(String[] args) throws InterruptedException {
        Thread.sleep(10000);
        System.out.println("Start");
        int tasksLimit = 10_000;
        List<Runnable> tasks = TasksFactory.createSumAndPrintTasks(tasksLimit);

        int threadsLimit = 16;
        ScheduledThreadPoolExecutor scheduledExecutor = (ScheduledThreadPoolExecutor) Executors.newScheduledThreadPool(threadsLimit);
        Stats.printThreadPoolStats(scheduledExecutor);
        for (int i = 0; i < tasks.size(); i++) {
            Runnable task = tasks.get(i);
            scheduledExecutor.schedule(task, i, TimeUnit.MILLISECONDS);
        }

        int initialDelay = 1;
        int period = 1;
        scheduledExecutor.scheduleAtFixedRate(tasks.get(0), initialDelay, period, TimeUnit.MINUTES);

        System.out.println("Shutdown");
        LifeCycle.shutDownExecutor(scheduledExecutor);

        // Verify thread pool stats
        Stats.printThreadPoolStats(scheduledExecutor);
        System.out.println("End");
        Thread.sleep(10000);
    }
}
