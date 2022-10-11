import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        // TODO
        // Shopping cart that is randomly generated/rand for time spent
        // shopping and rand for time spent at checkout
        this.bakery = bakery;
        this.shoppingCart = new ArrayList<BreadType>();
        this.rnd = new Random();
        this.shopTime = rnd.nextInt(5);
        this.checkoutTime = rnd.nextInt(5);
        fillShoppingCart();
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        float sales = 0;
        for (int i = 0; i < shoppingCart.size(); i++) {
            System.out.println("Customer(" +  hashCode() + ") " + " has entered the store.");
            try {
                Thread.sleep(shopTime);
            } catch (Exception e) {
                e.printStackTrace();
            }
            if (shoppingCart.get(i) == BreadType.RYE) {	   
                try { 			
                    this.bakery.ryeWait.acquire();
                    this.bakery.takeBread(BreadType.RYE);
                    this.bakery.ryeWait.release();
                    sales += BreadType.RYE.getPrice();
                    System.out.println("Customer(" +  hashCode() + ") " + "removed  " + BreadType.RYE + " off the shelf and purchased");
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            else if (shoppingCart.get(i) == BreadType.SOURDOUGH) {	  
                try {  			
                    this.bakery.sourdoughWait.acquire();
                    this.bakery.takeBread(BreadType.SOURDOUGH);
                    this.bakery.sourdoughWait.release();
                    sales += BreadType.SOURDOUGH.getPrice();
                    System.out.println("Customer(" +  hashCode() + ") " + "removed  " + BreadType.SOURDOUGH + " off the shelf and purchased");
                }
                catch (Exception e) {
                    e.printStackTrace();
                }
            }
            else {  
                try {  			
                    this.bakery.wonderWait.acquire();
                    this.bakery.takeBread(BreadType.WONDER);
                    this.bakery.wonderWait.release();
                    sales += BreadType.WONDER.getPrice();
                    System.out.println("Customer(" +  hashCode() + ") " + "removed  " + BreadType.WONDER + " off the shelf and purchased");
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        try {
            this.bakery.checkoutWait.acquire();	    
	    	System.out.println("Customer(" + hashCode() + ") has left the store.");
	    	this.bakery.addSales(sales);
	    	Thread.sleep(checkoutTime);
	    	this.bakery.checkoutWait.release();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime="
                + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}