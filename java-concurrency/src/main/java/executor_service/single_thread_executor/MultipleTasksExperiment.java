package executor_service.single_thread_executor;

import common.LifeCycle;
import tasks.TasksFactory;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class MultipleTasksExperiment {

    public static void main(String[] args) throws InterruptedException {
        Thread.sleep(10000);
        System.out.println("Start");
        int limit = 10_000;
        List<Runnable> tasks = TasksFactory.createSumAndPrintTasks(limit);
        ExecutorService singleThreadExecutor = Executors.newSingleThreadExecutor();

        for (Runnable task : tasks) {
            singleThreadExecutor.execute(task);
        }

        System.out.println("Shutdown");
        LifeCycle.shutDownExecutor(singleThreadExecutor);
        System.out.println("End");
        Thread.sleep(10000);
    }
}
