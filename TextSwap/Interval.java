// Name: "Jessica Noel 10445079"
// Assignment 1
// Course: CS511-Concurrent Programming
//Pledge: "I pledge my honor that I have abided by the Stevens Honor System."

public class Interval {
    private int x;
    private int y;

    public Interval(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return this.x;
    }

    public int getY() {
        return this.y;
    }

    public String toString() {
        return "(" + this.x + ", " + this.y + ")";
    }
}