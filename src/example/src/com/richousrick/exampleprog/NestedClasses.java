package com.richousrick.exampleprog;

public class NestedClasses {

    public NestedClass sub;
    public InnerClass inner;

    public NestedClasses() {
        this.sub = new NestedClass(Core.numDeclaredClasses++);
        this.inner = new InnerClass();
    }

    static class NestedClass {
        public int id;

        public NestedClass(int id) {
            this.id = id;
        }
    }

    class InnerClass{
        @Override
        public String toString() {
            return "ID: " + NestedClasses.this.sub;
        }
    }

}
