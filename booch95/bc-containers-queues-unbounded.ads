with BC.Support.Unbounded;
generic
   with package Unb_Queue_Nodes is new 
     BC.Support.Unbounded(Item,Item_Ptr);
package BC.Containers.Queues.Unbounded is

   type Unb_Queue is new Queue with private;
    -- This Queue exhibits unlimited growth and collapsing, limited only by
    -- available memory.  Assignment is "deep".

   procedure Clear  (Obj : in out Unb_Queue);
   procedure Append (Obj : in out Unb_Queue; Elem : Item);
   procedure Pop    (Obj : in out Unb_Queue);
   procedure Remove (Obj : in out Unb_Queue; From : Natural);
   function Length  (Obj : in Unb_Queue) return Natural;
   function Is_Empty(Obj : in Unb_Queue) return Boolean;
   function Front   (Obj : in Unb_Queue) return Item;
   function Front   (Obj : in Unb_Queue) return Item_Ptr;
   function Location(Obj : in Unb_Queue; Elem : Item) return Natural;

   function "="(Left, Right : in Unb_Queue) return boolean;
   function Cardinality(Obj : in Unb_Queue) return Integer;
   procedure Purge(Obj : in out Unb_Queue);
   procedure Add(Obj : in out Unb_Queue; Elem : in out Item);

private

   function Item_At(Obj : in Unb_Queue; Index : in Natural) return Item_Ptr;

   type Unb_Queue is new Queue with record
      Rep : Unb_Queue_Nodes.Unb_Node_Ref := new Unb_Queue_Nodes.Unb_Node;
   end record;

   procedure Initialize( Obj : in out Unb_Queue);
   procedure Adjust(Obj : in out Unb_Queue);
   procedure Finalize(Obj : in out Unb_Queue);
 
end BC.Containers.Queues.Unbounded;

