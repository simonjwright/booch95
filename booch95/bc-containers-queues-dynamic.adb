with Unchecked_Deallocation;
package body BC.Containers.Queues.Dynamic is

   function Create(Size : Positive) return Dyn_Queue is
      Temp : Dyn_Queue;
   begin
      Temp.Rep := Dyn_Queue_Nodes.Create(Size);
      return Temp;
   end Create;

   function "="(Left, Right : Dyn_Queue) return Boolean is
      use Dyn_Queue_Nodes;
   begin
      return Left.Rep.all = Right.Rep.all;
   end "=";

   procedure Clear  (Obj : in out Dyn_Queue) is
   begin
      Dyn_Queue_Nodes.Clear(Obj.Rep.all);
   end Clear;

   procedure Append (Obj : in out Dyn_Queue; Elem : Item) is
   begin
      Dyn_Queue_Nodes.Append(Obj.Rep.all, Elem);
   end Append;

   procedure Pop    (Obj : in out Dyn_Queue) is
   begin
      Dyn_Queue_Nodes.Remove(Obj.Rep.all, 1);
   end Pop;

   procedure Remove (Obj : in out Dyn_Queue; From : Natural) is
   begin
      Dyn_Queue_Nodes.Remove(Obj.Rep.all, From);
   end Remove;

   function Length  (Obj : Dyn_Queue) return Natural is
   begin
      return Dyn_Queue_Nodes.Length(Obj.Rep.all);
   end Length;

   function Is_Empty(Obj : Dyn_Queue) return Boolean is
   begin
      return Dyn_Queue_Nodes.Length(Obj.Rep.all) = 0;
   end Is_Empty;

   function Front   (Obj : Dyn_Queue) return Item is
   begin
      return Dyn_Queue_Nodes.First(Obj.Rep.all);
   end Front;

   function Front   (Obj : in Dyn_Queue) return Item_Ptr is
   begin
      return Dyn_Queue_Nodes.First(Obj.Rep.all'access);
   end Front; 

   function Location(Obj : Dyn_Queue; Elem : Item) return Natural is
   begin
      return Dyn_Queue_Nodes.Location(Obj.Rep.all'access, Elem);
   end Location;

   procedure Preallocate   (Obj : in out Dyn_Queue; Size : Natural) is
   begin
      Dyn_Queue_Nodes.Preallocate(Obj.Rep.all, Size);
   end Preallocate;

   procedure Set_Chunk_Size(Obj : in out Dyn_Queue; Size : Natural) is
   begin
      Dyn_Queue_Nodes.Set_Chunk_Size(Obj.Rep.all, Size);
   end Set_Chunk_Size;

   function Chunk_Size(Obj : Dyn_Queue) return Natural is
   begin
      return Dyn_Queue_Nodes.Chunk_Size(Obj.Rep.all);
   end Chunk_Size;

   procedure Purge(Obj : in out Dyn_Queue) is
   begin
      Dyn_Queue_Nodes.Clear(Obj.Rep.all);
   end Purge;

   procedure Add(Obj : in out Dyn_Queue; Elem : in out Item) is
   begin
      Dyn_Queue_Nodes.Append(Obj.Rep.all, Elem);
   end Add;

   function Cardinality(Obj : Dyn_Queue) return Integer is
   begin
      return Dyn_Queue_Nodes.Length(Obj.Rep.all);
   end Cardinality;

   function Item_At(Obj : in Dyn_Queue; Index : in Natural) return Item_Ptr is
      TObj : aliased Dyn_Queue_Nodes.Dyn_Node := Obj.Rep.all;
   begin
      return Dyn_Queue_Nodes.Item_At(TObj'access, Index);
   end Item_at;

   procedure Initialize( Obj : in out Dyn_Queue) is
   begin
      Obj.Rep := Dyn_Queue_Nodes.Create;
   end Initialize;

   procedure Adjust(Obj : in out Dyn_Queue) is
   begin
      Obj.Rep := Dyn_Queue_Nodes.Create(Obj.Rep.all);
   end Adjust;

   procedure Finalize(Obj : in out Dyn_Queue) is
   begin
      Dyn_Queue_Nodes.Clear(Obj.Rep.all);
   end Finalize;

end BC.Containers.Queues.Dynamic;

