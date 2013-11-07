
class MObject {
    public String repr() {return "<MObject>"; }
    public void print() {System.out.println(this.repr()); }
}

class Lock {}

class T extends Thread {
    
    public Lock lock;
    public List store;

    public T() { }
    
    public T(Lock lock, List store) {this.lock = lock; this.store = store; } }


class List extends MObject {
    public List(Lock l) { }
    public Bool isnil(Lock l) {synchronized (l) {return new Bool(); } }
    public List find(Lock l, Num n) {synchronized (l) {return new List(l); } }
    public Num get(Lock l, Num n) {synchronized (l) {return new Num(); } }
    public void set(Lock l, Num n, Num m) {synchronized (l) {} }
    public List expand(Lock l, Num n, Num m) {synchronized (l) {return new List(l); } }
    public Num read(Lock l) {synchronized (l) {return new Num(); } }
    public void write(Lock l, Num m) {synchronized (l) {} }
    public Bool isPair(Lock l, Num n, Num m) {synchronized (l) {return new Bool(); } }
    public String repr() {return "<Not a concrete List>"; }
}

class Pair extends List {

    public Num key;
    public Num value;
    public List nxt;

    public Pair(Lock l, Num key, Num value, List nxt) {
        super(l);
        synchronized (l) {
            this.key = key;
            this.value = value;
            this.nxt = nxt;
        }
    }

    public Bool isnil(Lock l) {synchronized (l) {return new False(); } }
    public Num read(Lock l) {synchronized (l) {return this.value; } }
    public void write(Lock l, Num m) {synchronized (l) {this.value = m; } }
    public List find(Lock l, Num n) {synchronized (l) {return (List) this.key.isequal(n).ite(this, this.nxt.find(l, n)); } }
    public Num get(Lock l, Num n) {synchronized (l) {return this.find(l, n).read(l); } }
    public void set(Lock l, Num n, Num m) {synchronized (l) {this.find(l, n).write(l, m); } }
    public List expand(Lock l, Num n, Num m) {synchronized (l) {return new Pair(l, n, m, this); } }
    public String letter(Num n) {
        String alphabet = "abcdefhijklmnopqrstuvwxyz";
        return Character.toString(alphabet.charAt(n.number()));
    }

    public String repr() {return "(" + this.letter(this.key) + "," + this.value.number() + ")" + this.nxt.repr(); } }


class Nil extends List {
    public Nil(Lock l) { super(l); }
    public Bool isnil(Lock l) {synchronized (l) {return new True(); } }
    public List expand(Lock l, Num n, Num m) {synchronized (l) {return new Pair(l, n, m, this); } }
    public String repr() {return "<nil>"; } }

class Bool extends MObject {
    public Object ite(Object x, Object y) {return new Object(); }
    public String repr() {return (String) this.ite("true", "false"); }
    public void print() {System.out.println(this.repr()); }
}




class True extends Bool {
    public Object ite(Object x, Object y) { return x; }
    
}

class False extends Bool {
    public Object ite(Object x, Object y) { return y; }
}



class Num extends MObject {
    public Bool iszero() {return new Bool(); }
    public Bool notzero(){return new Bool(); }
    public Num pred() {return new Num(); }
    public Num succ() {return new Num(); }
    public String repr() {return "<Not a concrete nubmer>"; }
    public Bool isequal(Num n) {return new False(); }
    public Integer number() { return -1; }
}

class NotZero extends Num {
    public Num pd;
    public NotZero(Num n) {this.pd = n; }
    public Bool iszero() {return new False(); }
    public Bool notzero() { return new True(); }
    public Num pred() {return this.pd; }
    public Num succ() {return new NotZero(this); }
    public String repr() {return ("1" + this.pred().repr()); }
    public Bool isequal(Num n) {return (Bool) n.iszero().ite(new False(), this.pred().isequal(n.pred())); }
    public Integer number() { return 1+(this.pd.number()); }
}

class Zero extends Num {
    public Bool iszero() {return new True(); }
    public Bool notzero() { return new False(); }
    public Num pred() {return this; }
    public Num succ() {return new NotZero(this); }
    public String repr() {return "0"; }
    public Bool isequal(Num n) {return n.iszero(); }
    public Integer number() { return 0; }
}
