import List "mo:base/List";
import Order "mo:base/Order";
import Prelude "mo:base/Prelude";
import RBTree "mo:functional-rbtree/FunctionalRBTree";
import Trie "mo:base/Trie";

module {
    public type SortedUniqueIndex<K, V> = RBTree.Tree<K, V>;
    public type SortedMultiIndex<K, V>  = RBTree.Tree<K, List.List<V>>;
    
    func addToSorted<K, V>(k : K, idx : SortedMultiIndex<K,V>, ord : OrderFunc<K>, v : V) : SortedMultiIndex<K,V> {
        switch(RBTree.get(k, ord, idx)) {
            case (null) {
                return RBTree.put(k, ord, List.make(v), idx).1;
            };
            case (?existing) {
                return RBTree.put(k, ord, List.push(v, existing), idx).1;
            };
        };
    };

    func removeFromSorted<K, V>(k : K, idx : SortedMultiIndex<K,V>, ord : OrderFunc<K>, anyButValue : (V) -> Bool) : SortedMultiIndex<K,V> {
        switch(RBTree.get(k, ord, idx)) {
            case (null) {
                return idx;
            };
            case (?existing) {
                return RBTree.put(k, ord, List.filter<V>(existing, anyButValue), idx).1;
            };
        };
    };
    
    public type HashUniqueIndex<K, V> = Trie.Trie<K, V>;
    public type HashMultiIndex<K, V>  = Trie.Trie<K, List.List<V>>;

    public type Filter<K, V> = (K, V) -> ?V;

    public type Index<K, V> = object {
        add    : (K, V) -> ();
    };

    public type MultiIndex<K, V> = object {
        getAllWhere : (Filter<V, V>) -> [(K, V)];
        remove      : (K, V)         -> ?V;
    } and Index<K, V>;

    public type UniqueIndex<K, V> = object {
        get    : K -> ?V;
        remove : K -> ?V;
    } and Index<K, V>;

    // Wrap V in a mutable cell;
    public type Cell<V> = {var v : V};

    // What is our goal?
    // 1. We want to put / get fluently
    // 2. We want to interact with idx fluently

    public type Store<K, V> = object {
        get :  K     -> ?V;
        put : (K, V) -> ?V;
    };

    public type Mapper<K, V, K2> = (K, V) -> (K2);
    public type OrderFunc<K> = (K, K) -> Order.Order;

    public type DaoFactory<K, V> = object {
        addSortedUniqueIndex : <K2>(name : Text, ord : OrderFunc<K2>, mapper : Mapper<K, V, K2>) -> Cell<SortedUniqueIndex<K2, K>>;
        addSortedMultiIndex  : <K2>(name : Text, ord : OrderFunc<K2>, mapper : Mapper<K, V, K2>) -> Cell<SortedMultiIndex<K2, K>>;

        //addHashUniqueIndex  : <K2, V2>(ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) -> Cell<HashUniqueIndex<K2, V2>>;
        //addHashMultiIndex   : <K2, V2>(ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) -> Cell<HashMultiIndex<K2, V2>>;

        build : () -> Store<K, V>;
    };

    public type IndexAction = {#add; #remove};
    public type IdexControllerFunc<K, V> = (K, V, IndexAction) -> ();

    public func daoFactory<K, V>(keyBuilder : (K) -> Trie.Key<K>, keyEqual : (K, K) -> Bool) : DaoFactory<K, V> {

        return object {
            var indicies : List.List<(Text, IdexControllerFunc<K, V>)> = List.nil();

            public func addSortedUniqueIndex<K2>(name : Text, ord : OrderFunc<K2>, mapper : Mapper<K, V, K2>) : Cell<SortedUniqueIndex<K2, K>> {
                let idx : Cell<RBTree.Tree<K2, K>> = {var v = RBTree.empty()};

                let indexController = func(k : K, v : V, action : IndexAction) {
                    let idxKey = mapper(k, v);
                    switch(action) {
                        case (#add) {
                            idx.v := RBTree.put<K2, K>(idxKey, ord, k, idx.v).1;
                        };
                        case (#remove) {
                            idx.v := RBTree.remove<K2, K>(idxKey, ord, idx.v).1;
                        }
                    }
                };
        
                // Register our indexer
                indicies := List.push((name, indexController), indicies);
                return idx;
            };

            public func addSortedMultiIndex<K2>(name : Text, ord : OrderFunc<K2>, mapper : Mapper<K, V, K2>) : Cell<SortedMultiIndex<K2, K>> {
                let idx : Cell<RBTree.Tree<K2, List.List<K>>> = {var v = RBTree.empty()};

                let indexController = func(k : K, v : V, action : IndexAction) {
                    let idxValue = mapper(k, v);
                    switch(action) {
                        case (#add) {
                            idx.v := addToSorted(idxValue, idx.v, ord, k);
                        };
                        case (#remove) {
                            let allButValue : (K) -> Bool = func (toCheck) {return not keyEqual(toCheck, k)};
                            idx.v := removeFromSorted<K2, K>(idxValue, idx.v, ord, allButValue);
                        }
                    }
                };
        
                // Register our indexer
                indicies := List.push((name, indexController), indicies);
                return idx;
            };

            public func build() : Store<K, V> {
                Prelude.nyi();
            };
        }
    };
}