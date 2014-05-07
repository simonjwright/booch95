--  Copyright 2003-2014 Simon Wright <simon@pushface.org>

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

with Ada.Numerics.Generic_Elementary_Functions;

package body BC.Support.Statistics is

   package Math
   is new Ada.Numerics.Generic_Elementary_Functions (Long_Float);

   ---------
   -- Add --
   ---------

   procedure Add (Datum : Long_Float; To : in out Instance) is
   begin
      To.Count := To.Count + 1;
      To.Min := Long_Float'Min (To.Min, Datum);
      To.Max := Long_Float'Max (To.Max, Datum);
      To.Summed_Data := To.Summed_Data + Datum;
      To.Summed_Squared_Data := To.Summed_Squared_Data + Datum * Datum;
   end Add;

   -----------
   -- Count --
   -----------

   function Count (Of_Instance : Instance) return Natural is
   begin
      return Of_Instance.Count;
   end Count;

   ----------
   -- Mean --
   ----------

   function Mean (Of_Instance : Instance) return Long_Float is
   begin
      return Of_Instance.Summed_Data / Long_Float (Of_Instance.Count);
   end Mean;

   --------------
   -- Variance --
   --------------

   function Variance (Of_Instance : Instance) return Long_Float is
   begin
      if Of_Instance.Count < 2 then
         return 0.0;
      else
         return
           Long_Float'Max
             ((Of_Instance.Summed_Squared_Data
               - Of_Instance.Summed_Data * Of_Instance.Summed_Data
               / Long_Float (Of_Instance.Count)),
              0.0)
           / Long_Float (Of_Instance.Count - 1);
      end if;
   end Variance;

   ------------------------
   -- Standard_Deviation --
   ------------------------

   function Sigma (Of_Instance : Instance) return Long_Float is
   begin
      return Math.Sqrt (Variance (Of_Instance));
   end Sigma;

   ---------
   -- Min --
   ---------

   function Min (Of_Instance : Instance) return Long_Float is
   begin
      return Of_Instance.Min;
   end Min;

   ---------
   -- Max --
   ---------

   function Max (Of_Instance : Instance) return Long_Float is
   begin
      return Of_Instance.Max;
   end Max;

end BC.Support.Statistics;
