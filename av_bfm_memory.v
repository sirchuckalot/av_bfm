/*
* Avalon BFM-based Memory Model
* Copyright (C) 2019      Charley Picker <charleypicker@yahoo.com>
*               2013-2015 Olof Kindgren <olof.kindgren@gmail.com>
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

module av_bfm_memory
 #(//Avalon parameters
   parameter dw = 32,
   parameter aw = 32,
   parameter burstw = 8,
   parameter DEBUG = 0,
   // Memory parameters
   parameter memory_file = "",
   parameter mem_size_bytes = 32'h0000_8000, // 32KBytes
   parameter rd_min_delay = 0,
   parameter rd_max_delay = 4)
  (input              av_clk_i,
   input              av_rst_i,

   input [aw-1:0]     av_address_i,
   input [dw-1:0]     av_writedata_i,
   input [dw/8-1:0]   av_byteenable_i,
   input [burstw-1:0] av_burstcount_i,
   input              av_write_i,
   input              av_read_i,

   output             av_waitrequest_o,
   output             av_readdatavalid_o,
   output [1:0]       av_response_o,
   output [dw-1:0]    av_readdata_o);

`include "av_common.v"

   localparam bytes_per_dw = (dw/8);
   localparam mem_words    = (mem_size_bytes/bytes_per_dw);

   parameter ADR_LSB       = $clog2(bytes_per_dw);

   //Counters for read and write accesses
   integer reads  = 0;
   integer writes = 0;

   // synthesis attribute ram_style of mem is block
   reg [dw-1:0] mem [ 0 : mem_words-1 ]   /* verilator public */ /* synthesis ram_style = no_rw_check */;

   av_bfm_slave
     #(.aw (aw),
       .dw (dw),
       .burstw(burstw),
       .DEBUG (DEBUG))
   bfm0
     (.av_clk_i           (av_clk_i),
      .av_rst_i           (av_rst_i),

      .av_address_i       (av_address_i),
      .av_writedata_i     (av_writedata_i),
      .av_byteenable_i    (av_byteenable_i),
      .av_burstcount_i    (av_burstcount_i),
      .av_write_i         (av_write_i),
      .av_read_i          (av_read_i),

      .av_waitrequest_o   (av_waitrequest_o),
      .av_readdatavalid_o (av_readdatavalid_o),
      .av_response_o      (av_response_o),
      .av_readdata_o      (av_readdata_o));

   reg [aw-1:0] address;
   reg [dw-1:0] data;

   integer      i;
   integer      delay;
   integer      seed;

   always begin
        bfm0.init();
        address = bfm0.address; //Fetch start address

        if(bfm0.op === WRITE)
            writes = writes + 1;
        else
            reads = reads + 1;
        while(bfm0.has_next) begin
            //Set error on out of range accesses
            if(address[31:ADR_LSB] > mem_words) begin
                $display("%0d : Error : Attempt to access %x, which is outside of memory", $time, address);
                bfm0.error_response();
            end else begin
                if(bfm0.op === WRITE) begin
                    bfm0.write_ack(data);
                    if(DEBUG) $display("%d : ram Write 0x%h = 0x%h %b", $time, address, data, bfm0.mask);
                    for(i=0;i < dw/8; i=i+1)
                        if(bfm0.mask[i])
                        mem[address[31:ADR_LSB]][i*8+:8] = data[i*8+:8];
                end else begin
                    data = {aw{1'b0}};
                    for(i=0;i < dw/8; i=i+1)
                        if(bfm0.mask[i])
                            data[i*8+:8] = mem[address[31:ADR_LSB]][i*8+:8];
                    if(DEBUG) $display("%d : ram Read  0x%h = 0x%h %b", $time, address, data, bfm0.mask);
                    delay = $dist_uniform(seed, rd_min_delay, rd_max_delay);
                    repeat(delay) @(posedge av_clk_i);
                    bfm0.read_ack(data);
                end
            end
            if(bfm0.cycle_type === BURST_CYCLE)
                address = av_next_adr(address, CTI_INC_BURST, BTE_LINEAR, dw);
        end
   end

endmodule // av_bfm_memory
