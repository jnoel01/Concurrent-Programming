#define N 3 /* Number of Washing Machines */
#define C 10 /* Number of Washing Machines */

byte permToProcess[N]
byte doneProcessing[N]  
byte station0 = 1
byte station1 = 1
byte station2 = 1

inline acquire(s) {
  atomic {
    s > 0 ->
      s--
  }
}

inline release(s) {
  s++
}

proctype Car() {
  end1:
  do
    ::station0 == 1 ->
      do
        acquire(station0);
        release(permToProcess[0]);
        acquire(doneProcessing[0]);
        acquire(station1);
      progress1:
        release(station0);
      od
    
    :: station1 == 1 ->
      do
        release(permToProcess[1]);
        acquire(doneProcessing[1]);
      progress2:
        acquire(station2);
        release(station1);
      od

    :: station2 == 1 ->
      do
        release(permToProcess[2]);
        acquire(doneProcessing[2]);
      progress3:
        release(station2);
      od
  od
}

proctype Machine(int i)  { 
       end1:
  do
    :: /* Wait for car to arrive */
       acquire(permToProcess[i]);
      /* Process car when it has arrived */
       release(doneProcessing[i]);
  od
} 


init {
  byte i;

  for (i:0..(N-1)) {
     permToProcess[i]=0;
     doneProcessing[i]=0;
   }
  
 atomic {
   for (i:1 .. C ) {
     run Car();
   }
   for (i:0 ..(N-1)) {
     run Machine(i);
   }
 }
}
