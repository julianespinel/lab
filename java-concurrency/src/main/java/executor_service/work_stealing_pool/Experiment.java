package executor_service.work_stealing_pool;

import common.LifeCycle;
import tasks.TasksFactory;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Experiment {

    public static void main(String[] args) throws InterruptedException {
        Thread.sleep(10000);
        System.out.println("Start");
        int tasksLimit = 10_000;
        List<Runnable> tasks = TasksFactory.createSumAndPrintTasks(tasksLimit);

        ExecutorService workStealingPool = Executors.newWorkStealingPool();
        for (Runnable task : tasks) {
            workStealingPool.execute(task);
        }
        System.out.println("Shutdown");
        LifeCycle.shutDownExecutor(workStealingPool);
        System.out.println("End");
        Thread.sleep(10000);
    }
}
