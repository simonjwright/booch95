-- The Ada 95 Booch Components (Version 1.0 beta 1)
-- Copyright (C)1994-1997 Grady Booch and David Weller.  All Rights Reserved.
-- 
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library 
--      for a copy.
--
-- This file contains the specifications for the bounded queue form.

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

