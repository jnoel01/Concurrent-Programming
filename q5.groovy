import java.util.concurrent.Semaphore;

Semaphore permToLoad = new Semaphore(1);
Semaphore doneLoading = new Semaphore(0);

Semaphore trackOne = new Semaphore(1);
Semaphore trackTwo = new Semaphore(1);

track = [trackOne, trackTwo];


100.times {
    int dir = (new Random()).nextInt(2);
    Thread.start { //Passenger Train
        track[dir].acquire();
        //println("pass in");
        //println("pass out");
        track[dir].release();
    }
}

100.times {
    int dir = (new Random()).nextInt(2);
    Thread.start { //Freight Train
        trackOne.acquire();
        trackTwo.acquire();
        
        permToLoad.release();
        doneLoading.acquire();
        
        //println("freight in");
        //println("freight out");
    
        trackOne.release();
        trackTwo.release();
    }
}

Thread.start { //Loading machine
    while(true) {
        permToLoad.acquire();
        // load freight train
        doneLoading.release();
    }
}