package com.richousrick.exampleprog;

import java.util.ArrayList;

public class Core {

    public static int numDeclaredClasses = 0;

    public static void main(String[] args) {
        if(!new Core().testNestedIDIncrement()){
            throw new RuntimeException("Nested classes should have different IDs");
        }else{
            System.out.println("Success");
        }
    }

    /**
     * Tests nested classes ids increment
     * @return true if they increment as expected
     */
    public boolean testNestedIDIncrement(){
        ArrayList<NestedClasses> nested = new ArrayList<>();
        nested.add(new NestedClasses());
        nested.add(new NestedClasses());
        return !nested.get(1).toString().substring(2).equals((nested.get(0).sub.id+1)+"");
    }
}
