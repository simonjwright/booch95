--  Copyright 2016 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Task_Attributes;
with System;

package BC.Support.Caching is

   --  This package is intended for use to support the caching used in
   --  the Unbounded (and Unmanaged) forms of Containers.
   --
   --  The issue (see Feature Request #3) is the desire to allow
   --  concurrent iteration over an Unbounded Collection, provided
   --  that this is never attempted concurrently with updates.

   --  A single Cache is maintained per-task. The System.Address
   --  components are so because there is limited space in the Task
   --  Control Block for attributes, and it would be impractical to
   --  have a different Cache type for each Container
   --  instantiated. Using Containers should instantiate
   --  Address_To_Access_Conversions for their access-to-node type.
   --
   --  On retrieving the Cache, the first check is that it isn't in
   --  use for another Container instance, if so, it must be
   --  nulled. This will make certain usage patterns very inefficient
   --  (e.g. iterating over one container as part of the processing
   --  during iteration over another).
   --
   --  As with the original code, an Index of 0 means that the cache
   --  is invalid.
   type Cache is record
      Container : System.Address;
      Node      : System.Address;
      Index     : Natural;
   end record;

   Null_Cache : constant Cache := (Container => System.Null_Address,
                                   Node      => System.Null_Address,
                                   Index     => 0);

   package Cache_Attributes
     is new Ada.Task_Attributes (Attribute     => Cache,
                                 Initial_Value => Null_Cache);

   generic
      type Container is private;
      type Node is private;
      type Node_Ref is access Node;
   package Cache_Manager is
      subtype Cache_P is Cache_Attributes.Attribute_Handle;

      function Get_Cache (For_The_Container : Container) return Cache_P;

      function Get_Node_Ref (From : Cache_P) return Node_Ref;

      procedure Update (The_Cache : Cache_P;
                        With_Node : Node_Ref;
                        And_Index : Natural);
   end Cache_Manager;

end BC.Support.Caching;
