package tasks;

import java.util.ArrayList;
import java.util.List;

public class TasksFactory {

    public static List<Runnable> createSumAndPrintTasks(int limit) {
        List<Runnable> list = new ArrayList<>(limit);
        for (int i = 0; i < limit; i++) {
            String name = "Thread " + (i + 1);
            Runnable task = new SumAndPrint(name);
            list.add(task);
        }
        return list;
    }
}
