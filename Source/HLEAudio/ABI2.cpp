/*
Copyright (C) 2003 Azimer
Copyright (C) 2001,2006 StrmnNrmn

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

//
//	N.B. This source code is derived from Azimer's Audio plugin (v0.55?)
//	and modified by StrmnNrmn to work with Daedalus PSP. Thanks Azimer!
//	Drop me a line if you get chance :)
//

#include "stdafx.h"

#include <string.h>

#include "audiohle.h"
#include "AudioHLEProcessor.h"

#include "Math/MathUtil.h"

#include "Debug/DBGConsole.h"

static void SPNOOP( AudioHLECommand command )
{
	#ifdef DAEDALUS_DEBUG_CONSOLE
	DBGConsole_Msg( 0, "AudioHLE: Unknown/Unimplemented Audio Command %i in ABI 2", command.cmd );
	#endif
}

bool isMKABI {false};
bool isZeldaABI {false};

static u32 gEnv_t3 {}, gEnv_s5 {}, gEnv_s6 {};
static u16 env[8] {};

inline u16 Sample_Mask( u32 x )
{
	return (u16)( x & 0xffff );
}

static void LOADADPCM2( AudioHLECommand command )
{
	// Loads an ADPCM table - Works 100% Now 03-13-01
	u32		address(command.Abi2LoadADPCM.Address );// + gAudioHLEState.Segments[(command.cmd1>>24)&0xf];
	u16		count( command.Abi2LoadADPCM.Count );

	gAudioHLEState.LoadADPCM( address, count );
}



inline int Scale16( s16 in, int vscale )
{
	return ((int)in*vscale)>>16;
}

static void Decode4_Scale( int (&inp1)[8], u32 icode_a, u32 icode_b, int vscale )
{
	inp1[0] = Scale16( (s16)((icode_a&0xC0) <<  8), vscale );
	inp1[1] = Scale16( (s16)((icode_a&0x30) << 10), vscale );
	inp1[2] = Scale16( (s16)((icode_a&0x0C) << 12), vscale );
	inp1[3] = Scale16( (s16)((icode_a&0x03) << 14), vscale );
	inp1[4] = Scale16( (s16)((icode_b&0xC0) <<  8), vscale );
	inp1[5] = Scale16( (s16)((icode_b&0x30) << 10), vscale );
	inp1[6] = Scale16( (s16)((icode_b&0x0C) << 12), vscale );
	inp1[7] = Scale16( (s16)((icode_b&0x03) << 14), vscale );
}

static void Decode4( int (&inp1)[8], u32 icode_a, u32 icode_b )
{
	inp1[0] = (s16)((icode_a&0xC0) <<  8);
	inp1[1] = (s16)((icode_a&0x30) << 10);
	inp1[2] = (s16)((icode_a&0x0C) << 12);
	inp1[3] = (s16)((icode_a&0x03) << 14);
	inp1[4] = (s16)((icode_b&0xC0) <<  8);
	inp1[5] = (s16)((icode_b&0x30) << 10);
	inp1[6] = (s16)((icode_b&0x0C) << 12);
	inp1[7] = (s16)((icode_b&0x03) << 14);
}

static void Decode8_Scale( int (&inp1)[8], u32 icode_a, u32 icode_b, u32 icode_c, u32 icode_d, int vscale )
{
	inp1[0] = Scale16( (s16)((icode_a&0xF0) <<  8), vscale );
	inp1[1] = Scale16( (s16)((icode_a&0x0F) << 12), vscale );
	inp1[2] = Scale16( (s16)((icode_b&0xF0) <<  8), vscale );
	inp1[3] = Scale16( (s16)((icode_b&0x0F) << 12), vscale );
	inp1[4] = Scale16( (s16)((icode_c&0xF0) <<  8), vscale );
	inp1[5] = Scale16( (s16)((icode_c&0x0F) << 12), vscale );
	inp1[6] = Scale16( (s16)((icode_d&0xF0) <<  8), vscale );
	inp1[7] = Scale16( (s16)((icode_d&0x0F) << 12), vscale );
}

static void Decode8( int (&inp1)[8], u32 icode_a, u32 icode_b, u32 icode_c, u32 icode_d )
{
	inp1[0] = (s16)((icode_a&0xF0) <<  8);
	inp1[1] = (s16)((icode_a&0x0F) << 12);
	inp1[2] = (s16)((icode_b&0xF0) <<  8);
	inp1[3] = (s16)((icode_b&0x0F) << 12);
	inp1[4] = (s16)((icode_c&0xF0) <<  8);
	inp1[5] = (s16)((icode_c&0x0F) << 12);
	inp1[6] = (s16)((icode_d&0xF0) <<  8);
	inp1[7] = (s16)((icode_d&0x0F) << 12);
}

static void ADPCM2_Decode4( int (&inp1)[8], int (&inp2)[8], u32 inPtr, u8 code )
{
	u32 icode_a {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+0)^3]};
	u32 icode_b {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+1)^3]};
	u32 icode_c {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+2)^3]};
	u32 icode_d {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+3)^3]};

	if( code < 0xE )
	{
		int vscale(0x8000>>((0xE - code)-1));
		Decode4_Scale( inp1, icode_a, icode_b, vscale );
		Decode4_Scale( inp2, icode_c, icode_d, vscale );
	}
	else
	{
		Decode4( inp1, icode_a, icode_b );
		Decode4( inp2, icode_c, icode_d );
	}
}

static void ADPCM2_Decode8( int (&inp1)[8], int (&inp2)[8], u32 inPtr, u8 code )
{
	u32 icode_a {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+0)^3]};
	u32 icode_b {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+1)^3]};
	u32 icode_c {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+2)^3]};
	u32 icode_d {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+3)^3]};
	u32 icode_e {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+4)^3]};
	u32 icode_f {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+5)^3]};
	u32 icode_g {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+6)^3]};
	u32 icode_h {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr+7)^3]};

	if( code < 0xC )
	{
		int vscale(0x8000>>((0xC - code)-1));
		Decode8_Scale( inp1, icode_a, icode_b, icode_c, icode_d, vscale );
		Decode8_Scale( inp2, icode_e, icode_f, icode_g, icode_h, vscale );
	}
	else
	{
		Decode8( inp1, icode_a, icode_b, icode_c, icode_d );
		Decode8( inp2, icode_e, icode_f, icode_g, icode_h );
	}
}

static void ADPCM2_Loop( s32 (&a)[8], int (&i1)[8], const s16 * b1, const s16 * b2, s16 * out )
{
	int l1( a[6] );
	int l2( a[7] );

	const int scl( 2048 );

	a[0] = ((int)b1[0]*l1) + ((int)b2[0]*l2) + ((int)i1[0]*scl);
	a[1] = ((int)b1[1]*l1) + ((int)b2[1]*l2) + ((int)b2[0]*i1[0]) + ((int)i1[1]*scl);
	a[2] = ((int)b1[2]*l1) + ((int)b2[2]*l2) + ((int)b2[1]*i1[0]) + ((int)b2[0]*i1[1]) + ((int)i1[2]*scl);
	a[3] = ((int)b1[3]*l1) + ((int)b2[3]*l2) + ((int)b2[2]*i1[0]) + ((int)b2[1]*i1[1]) + ((int)b2[0]*i1[2]) + ((int)i1[3]*scl);
	a[4] = ((int)b1[4]*l1) + ((int)b2[4]*l2) + ((int)b2[3]*i1[0]) + ((int)b2[2]*i1[1]) + ((int)b2[1]*i1[2]) + ((int)b2[0]*i1[3]) + ((int)i1[4]*scl);
	a[5] = ((int)b1[5]*l1) + ((int)b2[5]*l2) + ((int)b2[4]*i1[0]) + ((int)b2[3]*i1[1]) + ((int)b2[2]*i1[2]) + ((int)b2[1]*i1[3]) + ((int)b2[0]*i1[4]) + ((int)i1[5]*scl);
	a[6] = ((int)b1[6]*l1) + ((int)b2[6]*l2) + ((int)b2[5]*i1[0]) + ((int)b2[4]*i1[1]) + ((int)b2[3]*i1[2]) + ((int)b2[2]*i1[3]) + ((int)b2[1]*i1[4]) + ((int)b2[0]*i1[5]) + ((int)i1[6]*scl);
	a[7] = ((int)b1[7]*l1) + ((int)b2[7]*l2) + ((int)b2[6]*i1[0]) + ((int)b2[5]*i1[1]) + ((int)b2[4]*i1[2]) + ((int)b2[3]*i1[3]) + ((int)b2[2]*i1[4]) + ((int)b2[1]*i1[5]) + ((int)b2[0]*i1[6]) + ((int)i1[7]*scl);

	for(u32 j {}; j<8; j++)
	{
		s16 r {Saturate<s16>( a[j^1] >> 11 )};
		a[j^1] = r;
		out[j]=r;		// XXXX endian issues
	}
}

static void ADPCM2( AudioHLECommand command )
{
	// Verified to be 100% Accurate...
	u8 Flags {(u8)((command.cmd0>>16)&0xff)};
	//u16 Gain=(u16)(command.cmd0&0xffff);	// XXXX Unused
	u32 Address {(command.cmd1 & 0xffffff)};// + gAudioHLEState.Segments[(command.cmd1>>24)&0xf];

	bool init( (Flags&0x1) != 0 );
	bool loop( (Flags&0x2) != 0 );
	bool decode4( (Flags & 0x4) != 0 );		// 4 bytes -> 16 output samples

	s16 * out( (s16 *)(gAudioHLEState.Buffer+gAudioHLEState.OutBuffer) );
	if(init)
	{
		memset(out,0,32);
	}
	else
	{
		u32		src_addr( loop ? gAudioHLEState.LoopVal : Address );
		memcpy(out,&rdram[src_addr],32);		// XXXX Endian issues?
	}

	u16 inPtr {};

	s32 a[8] = { 0,0,0,0,0,0,out[15],out[14] };		// XXXX Endian issues - should be 14/15^TWIDDLE?

	out+=16;
	short count {(short)gAudioHLEState.Count};
	while(count>0)
	{
		u8 idx_code {gAudioHLEState.Buffer[(gAudioHLEState.InBuffer+inPtr)^3]};
		inPtr++;

		u16 index((idx_code&0xf)<<4);
		u8	code(idx_code>>=4);

		s16 * book1 {(s16 *)&gAudioHLEState.ADPCMTable[index]};
		s16 * book2 {book1+8};

		// Decode inputs
		int inp1[8] {};
		int inp2[8] {};

		if( decode4 )
		{
			ADPCM2_Decode4( inp1, inp2, inPtr, code );
			inPtr+=4;
		}
		else
		{
			ADPCM2_Decode8( inp1, inp2, inPtr, code );
			inPtr+=8;
		}

		// Generate samples
		ADPCM2_Loop( a, inp1, book1, book2, out );
		ADPCM2_Loop( a, inp2, book1, book2, out+8 );

		out += 16;
		count-=32;
	}
	out-=16;
	memcpy(&rdram[Address],out,32);
}



static void MIXER2( AudioHLECommand command )
{
	// Needs accuracy verification...
	u16 dmemin( command.Abi2Mixer.DmemIn );
	u16 dmemout( command.Abi2Mixer.DmemOut );
	s32 gain( command.Abi2Mixer.Gain );
	u16	count( command.Abi2Mixer.Count * 16 );

	//printf( "Mixer: i:%04x o:%04x g:%08x (%d) c:%04x - %08x%08x\n", dmemin, dmemout, gain, s16(gain), count, command.cmd0, command.cmd1 );

	gAudioHLEState.Mixer( dmemout, dmemin, gain, count );		// NB - did mult gain by 2 above, then shifted by 16 inside mixer.
}

static void RESAMPLE2( AudioHLECommand command )
{
	u8 flags(command.Abi2Resample.Flags);
	u32 pitch(command.Abi2Resample.Pitch);
	u32 address(command.Abi2Resample.Address);// + gAudioHLEState.Segments[(command.cmd1>>24)&0xf];

	gAudioHLEState.Resample( flags, pitch, address );
}



// OK 26/03/19 - Wally

static void DEINTERLEAVE2( AudioHLECommand command )
{
	u16 count( command.Abi2Deinterleave.Count );
	u16 out( command.Abi2Deinterleave.Out );
	u16 in( command.Abi2Deinterleave.In );

	gAudioHLEState.Deinterleave( out, in, count );
}

static void INTERLEAVE2( AudioHLECommand command )  // Needs accuracy verification...
{
	u16	inR( command.Abi2Interleave.RAddr );
	u16	inL( command.Abi2Interleave.LAddr);
	u16 out( command.Abi2Interleave.OutAddr );
	u16 count( command.Abi2Interleave.Count );

	if (count != 0)
	{
		gAudioHLEState.Interleave( out, inL, inR, count );
	}
	else
	{
		gAudioHLEState.Interleave( inL, inR );
	}
}

// Readded 26/03/19 - Looks correct
// XXX Saturate should probably be replaced with Azi's function for consistency
static void ADDMIXER( AudioHLECommand command )
{

	u32 Count     = (command.cmd0 >> 12) & 0x00ff0;
	u32 InBuffer  = (command.cmd1 >> 16);
	u32 OutBuffer = command.cmd1 & 0xffff;

	s16 *inp  = (s16 *)(gAudioHLEState.Buffer + InBuffer);
	s16 *outp = (s16 *)(gAudioHLEState.Buffer + OutBuffer);
	s32 temp;
	for (u32 cntr = 0; cntr < Count; cntr+=2)
	{

	s32 temp = Saturate<s16>( *outp + *inp );
		 *outp = temp;
		outp++;	inp++;
	}
}

// XXX Looks Correct 26/03/19
// Again with saturate
static void HILOGAIN( AudioHLECommand command )
{
	u16 count {command.cmd0 & 0xffff};
	u16 out {(command.cmd1 >> 16) & 0xffff};
	s16 hi  {(s16)((command.cmd0 >> 4) & 0xf000)};
	u16 lo  {(command.cmd0 >> 20) & 0xf};

	s16 *src = (s16 *)(gAudioHLEState.Buffer+out);

	while( count )
	{
		s32 val = (s32)*src;
		s32 tmp = ((val * (s32)hi) >> 16) + (u32)(val * lo);
		*src++ = Saturate<s16>( tmp );
		count -= 2;
	}
}


static void UNKNOWN( AudioHLECommand command )
{
}

AudioHLEInstruction ABI2[0x20] =
{
    SPNOOP , ADPCM2, CLEARBUFF2, UNKNOWN, ADDMIXER, RESAMPLE2, UNKNOWN, SEGMENT2,
    SETBUFF2 , DUPLICATE2, DMEMMOVE2, LOADADPCM2, MIXER2, INTERLEAVE2, HILOGAIN, SETLOOP2,
    SPNOOP, DEINTERLEAVE2 , ENVSETUP1, ENVMIXER2, LOADBUFF2, SAVEBUFF2, ENVSETUP2, SPNOOP,
    HILOGAIN , SPNOOP, DUPLICATE2 , UNKNOWN    , SPNOOP  , SPNOOP    , SPNOOP  , SPNOOP
};

/* NOTES:

  FILTER/SEGMENT - Still needs to be finished up... add FILTER?
  UNKNOWWN #27	 - Is this worth doing?  Looks like a pain in the ass just for WaveRace64
*/
