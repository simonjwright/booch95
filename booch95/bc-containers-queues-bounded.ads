with BC.Support.Bounded;
generic
   Maximum_Size : Positive;
   with package Bnd_Queue_Nodes is 
    new BC.Support.Bounded(Item,Item_Ptr,Maximum_Size);
package BC.Containers.Queues.Bounded is

   -- See spec documentation for Queues unless overriding comment apply.

   type Bnd_Queue is new Queue with private;

   procedure Clear    (Obj : in out Bnd_Queue);
   procedure Append   (Obj : in out Bnd_Queue; Elem : Item);
   procedure Pop      (Obj : in out Bnd_Queue);
   procedure Remove   (Obj : in out Bnd_Queue; From : Natural);
   function Available (Obj : in Bnd_Queue) return Natural;
   -- Indicated number of empty "Item slots" left in Queue

   function Length    (Obj : in Bnd_Queue) return Natural;
   function Is_Empty  (Obj : in Bnd_Queue) return Boolean;
   function Front     (Obj : in Bnd_Queue) return Item;
   function Front     (Obj : in Bnd_Queue) return Item_Ptr;
   function Location  (Obj : in Bnd_Queue; Elem : Item) return Natural;

   function "="(Left, Right : in Bnd_Queue) return boolean;
   function Cardinality(Obj : in Bnd_Queue) return Integer;
   procedure Purge(Obj : in out Bnd_Queue);
   procedure Add(Obj : in out Bnd_Queue; Elem : in out Item);

private

   function Item_At (Obj : in Bnd_Queue; Index : Natural) return Item_Ptr;

   procedure Adjust(Obj : in out Bnd_Queue);

   procedure Finalize(Obj : in out Bnd_Queue);

   use Bnd_Queue_Nodes;

   type Bnd_Queue is new Queue with record
      Rep : Bnd_Queue_Nodes.Bnd_Node_Ref := new Bnd_Node;
   end record;

end BC.Containers.Queues.Bounded;

