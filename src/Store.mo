import List "mo:base/List";
import Order "mo:base/Order";
import Prelude "mo:base/Prelude";
import RBTree "mo:functional-rbtree/FunctionalRBTree";
import Trie "mo:base/Trie";

module {
    public type SortedUniqueIndex<K, V> = RBTree.Tree<K, V>;
    public type SortedMultiIndex<K, V>  = RBTree.Tree<K, List.List<V>>;

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

    public type F<K, V, K2, V2> = (K, V) -> (K2, V2);
    public type OrderFunc<K> = (K, K) -> Order.Order;

    public type StoreBuilder<K, V, O> = object {
        addSortedUniqueIndex : <K2, V2>(ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) -> Cell<SortedUniqueIndex<K2, V2>>;
        addSortedMultiIndex  : <K2, V2>(ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) -> Cell<SortedMultiIndex<K2, V2>>;
        
        build : () -> (O, Store<K, V>);
    };

    public type DaoFactory<K, V> = object {
        addSortedUniqueIndex : <K2, V2>(name : Text, ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) -> Cell<SortedUniqueIndex<K2, V2>>;
        //addSortedMultiIndex  : <K2, V2>(ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) -> Cell<SortedMultiIndex<K2, V2>>;

        //addHashUniqueIndex  : <K2, V2>(ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) -> Cell<HashUniqueIndex<K2, V2>>;
        //addHashMultiIndex   : <K2, V2>(ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) -> Cell<HashMultiIndex<K2, V2>>;

        build : () -> Store<K, V>;
    };

    public type IndexAction = {#add; #remove};
    public type IdexControllerFunc<K, V> = (K, V, IndexAction) -> ();

    public func daoFactory<K, V>(keyBuilder : (K) -> Trie.Key<K>, keyEqual : (K, K) -> Bool) : DaoFactory<K, V> {

        return object {
            var indicies : List.List<(Text, IdexControllerFunc<K, V>)> = List.nil();

            public func addSortedUniqueIndex<K2, V2>(name : Text, ord : OrderFunc<K2>, mapper : F<K, V, K2, V2>) : Cell<SortedUniqueIndex<K2, V2>> {
                let idx : Cell<RBTree.Tree<K2, V2>> = {var v = RBTree.empty()};

                let indexController = func(k : K, v : V, action : IndexAction) {
                    let values = mapper(k, v);
                    switch(action) {
                        case (#add) {
                            idx.v := RBTree.put<K2, V2>(values.0, ord, values.1, idx.v).1;
                        };
                        case (#remove) {
                            idx.v := RBTree.remove<K2, V2>(values.0, ord, idx.v).1;
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