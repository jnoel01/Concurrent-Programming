import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 200;
    private static final int CAPACITY = 50;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread;
    private float sales = 0;

    // TODO
    public Semaphore ryeWait = new Semaphore(1);
    public Semaphore sourdoughWait = new Semaphore(1);
    public Semaphore wonderWait = new Semaphore(1);
    public Semaphore checkoutWait = new Semaphore(4);
    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        int breadLeft = availableBread.get(bread);
        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1);
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        // TODO
        ExecutorService executor = Executors.newFixedThreadPool(CAPACITY);

        for (int i = 0; i < TOTAL_CUSTOMERS; i++) {
            Customer cust = new Customer(this);
            System.out.println(cust.toString());
            executor.execute(cust);
        }
        executor.shutdown();
        try {
            executor.awaitTermination(TOTAL_CUSTOMERS, TimeUnit.SECONDS);
        } catch (Exception e) {
            Thread.currentThread().interrupt();
        }
        System.out.println("Total sales for the day were: $" + this.sales);
    }
}
