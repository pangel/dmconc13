class T1 extends T {
 public T1(Lock l,List s) {
  super(l,s);
 }
 public void run() {
  this.store.set(this.lock,(new Zero()),this.store.get(this.lock,(new Zero())).succ());
  this.store.set(this.lock,(new Zero()),this.store.get(this.lock,(new Zero())).pred().succ());
  this.store.set(this.lock,(new Zero()),this.store.get(this.lock,(new Zero())).succ());
  this.store.set(this.lock,(new Zero()),this.store.get(this.lock,(new Zero())).pred().succ());
  this.store.set(this.lock,(new Zero()),this.store.get(this.lock,(new Zero())).pred().succ());
 }
}
 public class Compiled { 
    public static void main (String[] a) {
    List st = new Nil(new Lock()).expand(new Lock(),(new Zero()), (new Zero()));
    T t = (new T1(new Lock(),st));
    System.out.println("Store before:");
    st.print();
    t.run();
    System.out.println("Store after:");
    t.store.print();
    }}