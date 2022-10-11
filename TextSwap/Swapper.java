// Name: "Jessica Noel 10445079"
// Assignment 1
// Course: CS511-Concurrent Programming
//Pledge: "I pledge my honor that I have abided by the Stevens Honor System."

public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        // Write the specified content into the buffer.
        // Reference file Interval.java
        for (int i = 0; i < (interval.getY() - interval.getX()) + 1; ++i) {
            buffer[offset + i] = content.charAt(interval.getX() + i);
        }
    }
}