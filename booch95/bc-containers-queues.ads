generic
package Bc.Containers.Queues is

   type Queue is abstract new Container with private;

   -- A sequence in which items may be added from one end and removed from
   -- the opposite end.  This class is abstract and serves only to enforce
   -- the interfaces among classes.

   -- Operations of equality, inequality, and assignment are "deep" for
   -- all Queue forms

   procedure Clear   (Obj : in out Queue) is abstract;
   -- Empty the Queue of all items

   procedure Append  (Obj : in out Queue; Elem : Item) is abstract;
   -- Add the item to the back of the Queue; the Item itself is copied

   procedure Pop     (Obj : in out Queue) is abstract;
   -- Remove the Item from the front of the Queue

   procedure Remove  (Obj : in out Queue; From : Natural) is abstract;
   -- Remove the Item at the given Index (balking operation)

   function Length   (Obj : in Queue) return Natural is abstract;
   -- Returns total items in the Queue

   function Is_Empty (Obj : in Queue) return Boolean is abstract;
   -- Returns true iff no items are in the queue

   function Front    (Obj : in Queue) return Item is abstract;
   -- Return the item at the front of the Queue; the Item is _not_ removed

   function Front    (Obj : in Queue) return Item_Ptr is abstract;
   -- Return reference to item at the front of the Queue; 
   -- the Item is _not_ removed

   function Location (Obj : in Queue; Elem : in Item) return Natural
      is abstract;
   -- Returns the Index number where the Item is found.  A zero is
   -- returned on failure

   function "=" (Left, Right : access Queue'Class) return Boolean;

   procedure Copy (From : access Queue'Class; To : access Queue'Class);
   -- This operation MUST be called for dissimilar Queues in place of
   -- assignment.

private

   type Queue is abstract new Container with null record;

   procedure Purge (Obj : in out Queue);
   procedure Add (Obj : in out Queue; Elem : in out Item);

end Bc.Containers.Queues;


