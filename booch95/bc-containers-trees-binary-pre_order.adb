--  Copyright (C) 1994-2001 Grady Booch and Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

procedure BC.Containers.Trees.Binary.Pre_Order
  (T : Binary_Tree; Success : out Boolean) is
   Subtree : Binary_Tree;
   Result : Boolean;
begin
   Success := True;
   if not Is_Null (T) then
      Apply (T.Rep.Element, Result);
      if not Result then
         Success := False;
         return;
      end if;
      Subtree := T;
      Left_Child (Subtree);
      Pre_Order (Subtree, Result);
      if not Result then
         Success := False;
         return;
      end if;
      Subtree := T;
      Right_Child (Subtree);
      Pre_Order (Subtree, Result);
      if not Result then
         Success := False;
         return;
      end if;
   end if;
end BC.Containers.Trees.Binary.Pre_Order;
