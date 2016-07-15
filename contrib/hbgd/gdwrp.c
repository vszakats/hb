/*
 * GD graphic library low level (client api) interface code.
 *
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapistr.h"

/* NOTE: Do some initialization required by the GD headers. */
#if defined( HB_OS_WIN )
   #if ! defined( WIN32 )
      #define WIN32
   #endif
   #if ! defined( BGDWIN32 )
      #define BGDWIN32
   #endif
#endif

#include "gd.h"
#include "gdfontt.h"
#include "gdfonts.h"
#include "gdfontmb.h"
#include "gdfontl.h"
#include "gdfontg.h"

#define HB_GD_VERS( ma, mi, mu )  ( GD_MAJOR_VERSION > ma || ( GD_MAJOR_VERSION == ma && ( GD_MINOR_VERSION > mi || ( GD_MINOR_VERSION == mi && GD_RELEASE_VERSION >= mu ) ) ) )

#define IMAGE_JPEG  1
#define IMAGE_GIF   2
#define IMAGE_PNG   3
#define IMAGE_WBMP  4
#define IMAGE_GD    5

/* Internal function for handling pointers
 *
 * Code of Przemyslaw Czerpak for gdImagePtr
 * adapted also to gdFontPtr
 */

/* gdImage */

/* gdImage destructor, it's executed automatically */
static HB_GARBAGE_FUNC( hb_gdImage_Destructor )
{
   /* Retrieve image pointer holder */
   gdImagePtr * ptr = ( gdImagePtr * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( *ptr )
   {
      /* Destroy the image */
      gdImageDestroy( *ptr );

      /* set pointer to NULL to avoid multiple freeing */
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcGDimageFuncs =
{
   hb_gdImage_Destructor,
   hb_gcDummyMark
};

/* function returns gdImage pointer or NULL when wrong variable is
   passed or gdImage was freed before */
static gdImagePtr hb_parGdImage( int iParam )
{
   gdImagePtr * ptr = ( gdImagePtr * ) hb_parptrGC( &s_gcGDimageFuncs, iParam );

   return ptr ? *ptr : NULL;
}

static void * hb_isGdImage( int iParam )
{
   return hb_parptrGC( &s_gcGDimageFuncs, iParam );
}

/* function create in HVM stack return value item with gdImage pointer */
static void hb_retGdImage( gdImagePtr im )
{
   gdImagePtr * ptr = ( gdImagePtr * ) hb_gcAllocate( sizeof( gdImagePtr ),
                                                      &s_gcGDimageFuncs );

   *ptr = im;

   hb_retptrGC( ( void * ) ptr );
}

#if 0
/* function returns PHB_ITEM with gdImage pointer */
static PHB_ITEM hb_gdImageItemNew( gdImagePtr im )
{
   gdImagePtr * ptr = ( gdImagePtr * ) hb_gcAllocate( sizeof( gdImagePtr ),
                                                      &s_gcGDimageFuncs );

   *ptr = im;

   return hb_itemPutPtrGC( NULL, ( void * ) ptr );
}
#endif

/* gdFont */

/* gdFont destructor, it's executed automatically */
static HB_GARBAGE_FUNC( hb_gdFont_Destructor )
{
   /* Retrieve Font pointer holder */
   gdFontPtr * ptr = ( gdFontPtr * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( *ptr )
   {
      /* Destroy the Font */
#if 0
      /* do nothing, GD handles it directly and gdFontDestroy() not exists */
      gdFontDestroy( *ptr );
#endif

      /* set pointer to NULL to avoid multiple freeing */
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcGDfontFuncs =
{
   hb_gdFont_Destructor,
   hb_gcDummyMark
};

/* function returns gdFont pointer or NULL when wrong variable is
   passed or gdFont was freed before */
static gdFontPtr hb_parGdFont( int iParam )
{
   gdFontPtr * ptr = ( gdFontPtr * ) hb_parptrGC( &s_gcGDfontFuncs, iParam );

   return ptr ? *ptr : NULL;
}

static void * hb_isGdFont( int iParam )
{
   return hb_parptrGC( &s_gcGDfontFuncs, iParam );
}

/* function create in HVM stack return value item with gdFont pointer */
static void hb_retGdFont( gdFontPtr font )
{
   gdFontPtr * ptr = ( gdFontPtr * ) hb_gcAllocate( sizeof( gdFontPtr ),
                                                    &s_gcGDfontFuncs );

   *ptr = font;

   hb_retptrGC( ( void * ) ptr );
}

#if 0
/* function returns PHB_ITEM with gdFont pointer */
static PHB_ITEM hb_gdFontItemNew( gdFontPtr font )
{
   gdFontPtr * ptr = ( gdFontPtr * ) hb_gcAllocate( sizeof( gdFontPtr ),
                                                    &s_gcGDfontFuncs );

   *ptr = font;

   return hb_itemPutPtrGC( NULL, ( void * ) ptr );
}
#endif

static void * LoadImageFromFileObject( PHB_FILE fhandle, int sz )
{
   void * iptr = hb_xgrab( sz );

   hb_fileRead( fhandle, iptr, ( HB_SIZE ) sz, -1 );

   return iptr;
}

static void * LoadImageFromFile( const char * szFile, int * sz )
{
   HB_SIZE nSize;
   char *  iptr = ( char * ) hb_fileLoad( szFile, INT_MAX - 1, &nSize );

   *sz = iptr ? ( int ) nSize : 0;

   return ( void * ) iptr;
}

static void SaveImageToFileObject( PHB_FILE fhandle, const void * iptr, int sz )
{
   if( iptr )
      hb_fileWrite( fhandle, iptr, ( HB_SIZE ) sz, -1 );
}

static void SaveImageToHandle( HB_FHANDLE fhandle, const void * iptr, int sz )
{
   PHB_FILE file = hb_fileFromHandle( fhandle );

   SaveImageToFileObject( file, iptr, sz );

   hb_fileDetach( file );
}

static void SaveImageToFile( const char * szFile, const void * iptr, int sz )
{
   PHB_FILE fhandle = hb_fileExtOpen( szFile, NULL,
                                      FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE |
                                      FXO_TRUNCATE | FXO_SHARELOCK | FXO_NOSEEKPOS,
                                      NULL, NULL );

   if( fhandle )
   {
      if( iptr )
         hb_fileWrite( fhandle, iptr, ( HB_SIZE ) sz, -1 );
      hb_fileClose( fhandle );
   }
}

static void GDImageCreateFrom( int nType )
{
   int    sz;
   void * iptr;

   if( HB_ISCHAR( 1 ) )
      /* Retrieve image from file name */
      iptr = LoadImageFromFile( hb_parc( 1 ), &sz );
   else if( hb_isGdImage( 1 ) &&
            HB_ISNUM( 2 ) )
   {
      /* Retrieve image size */
      sz = hb_parni( 2 );

      /* Retrieve image pointer + size */
      iptr = hb_parGdImage( 1 );
   }
   else if( hb_fileParamGet( 1 ) &&
            HB_ISNUM( 2 ) )
   {
      /* Retrieve image size */
      sz = hb_parni( 2 );

      /* Retrieve image */
      iptr = LoadImageFromFileObject( hb_fileParamGet( 1 ), sz );
   }
   else if( HB_ISNUM( 1 ) &&
            HB_ISNUM( 2 ) )
   {
      PHB_FILE file = hb_fileFromHandle( hb_numToHandle( hb_parnintdef( 1, HB_STDIN_HANDLE ) ) );

      /* Retrieve image size */
      sz = hb_parni( 2 );

      /* Retrieve image */
      iptr = LoadImageFromFileObject( file, sz );

      hb_fileDetach( file );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( iptr && sz )
   {
      gdImagePtr im;

      /* Create Image */
      switch( nType )
      {
         case IMAGE_JPEG:
            im = gdImageCreateFromJpegPtr( sz, iptr );
            break;
         case IMAGE_GIF:
            im = gdImageCreateFromGifPtr( sz, iptr );
            break;
         case IMAGE_PNG:
            im = gdImageCreateFromPngPtr( sz, iptr );
            break;
         case IMAGE_WBMP:
            im = gdImageCreateFromWBMPPtr( sz, iptr );
            break;
         case IMAGE_GD:
            im = gdImageCreateFromGdPtr( sz, iptr );
            break;
         default:
            im = NULL;
      }

      /* Return image pointer */
      hb_retGdImage( im );
   }

   if( iptr )
      hb_xfree( iptr );
}

static void GDImageSaveTo( int nType )
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      int    sz = 0;
      void * iptr = NULL;
      int    level = 0, fg = 0;

      /* Get file name or an output handler or NIL it I want a return string */
      if( ! ( HB_ISNIL( 2 ) ||
              HB_ISCHAR( 2 ) ||
              hb_fileParamGet( 2 ) ||
              HB_ISNUM( 2 ) ) )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 0,
                               "Second argument must be NIL, a file handle or a string.",
                               HB_ERR_FUNCNAME, 1, hb_paramError( 2 ) );
         return;
      }

      /* Retrieve compression level */
      /* check if is numeric */
      if( ! ( HB_ISNIL( 3 ) ||
              HB_ISNUM( 3 ) ) )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 0,
                               "Third argument must be NIL or numeric.",
                               HB_ERR_FUNCNAME, 1, hb_paramError( 3 ) );
         return;
      }

      if( nType == IMAGE_JPEG )
      {
         /* check range */
         level = hb_parnidef( 3, -1 );
         if( ! ( level >= -1 && level <= 95 ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 0,
                                  "Compression level must be -1 (default) or a value between 0 and 95.",
                                  HB_ERR_FUNCNAME, 1, hb_paramError( 3 ) );
            return;
         }
      }
      else if( nType == IMAGE_PNG )
      {
         /* check range */
         level = hb_parnidef( 3, -1 );
         if( ! ( level >= -1 && level <= 9 ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 0,
                                  "Compression level must be -1 (default) or a value between 0 and 9.",
                                  HB_ERR_FUNCNAME, 1, hb_paramError( 3 ) );
            return;
         }
      }
      else if( nType == IMAGE_WBMP )
      {
         if( ! HB_ISNUM( 3 ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 0,
                                  "Foreground color nedeed",
                                  HB_ERR_FUNCNAME, 1, hb_paramError( 3 ) );
            return;
         }
         fg = hb_parni( 3 );
      }

      switch( nType )
      {
         case IMAGE_JPEG:
            /* Get image Ptr */
            iptr = gdImageJpegPtr( im, &sz, level );
            break;
         case IMAGE_GIF:
            /* Get image Ptr */
            iptr = gdImageGifPtr( im, &sz );
            break;
         case IMAGE_PNG:
            /* Get image Ptr */
            iptr = gdImagePngPtrEx( im, &sz, level );
            break;
         case IMAGE_WBMP:
            /* Get image Ptr */
            iptr = gdImageWBMPPtr( im, &sz, fg );
            break;
         case IMAGE_GD:
            /* Get image Ptr */
            iptr = gdImageGdPtr( im, &sz );
            break;
      }

      /* If I get a file name */
      if( HB_ISCHAR( 2 ) )
         SaveImageToFile( hb_parc( 2 ), iptr, sz );

      /* Write to file handle */
      else if( hb_fileParamGet( 2 ) )
         SaveImageToFileObject( hb_fileParamGet( 2 ), iptr, sz );
      else if( HB_ISNUM( 2 ) )
      {
         /* Write to std output or to a passed file */
         HB_FHANDLE fhandle = hb_numToHandle( hb_parnint( 2 ) );

         if( fhandle == FS_ERROR || fhandle == 0 )
            fhandle = HB_STDOUT_HANDLE;

         /* Write Image */
         SaveImageToHandle( fhandle, iptr, sz );
      }
      /* Return image as string */
      else
         hb_retclen( ( const char * ) iptr, ( HB_SIZE ) sz );

      /* Free memory */
      gdFree( iptr );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* WRAPPER FUNCTIONS */

HB_FUNC( HB_GD_VERSION )
{
   hb_storni( GD_MAJOR_VERSION, 1 );
   hb_storni( GD_MINOR_VERSION, 2 );
   hb_storni( GD_RELEASE_VERSION, 3 );
}

HB_FUNC( GDVERSION )
{
#if HB_GD_VERS( 2, 0, 34 )
   char szVer[ 30 ];
   hb_snprintf( szVer, sizeof( szVer ), "GD Version %s", GD_VERSION_STRING );
   hb_retc( szVer );
#elif HB_GD_VERS( 2, 0, 33 )
   hb_retc_const( "GD Version 2.0.33" );
#else
   hb_retc_const( "GD Version 2.0.28" );
#endif
}

HB_FUNC( GDVERSIONNUMBER )
{
#if HB_GD_VERS( 2, 0, 34 )
   hb_retni( GD_MAJOR_VERSION * 1000 + GD_MINOR_VERSION * 100 + GD_RELEASE_VERSION );
#elif HB_GD_VERS( 2, 0, 33 )
   hb_retni( 2033 );
#else
   hb_retni( 2028 );
#endif
}

/* IMAGE CREATION, DESTRUCTION, LOADING AND SAVING */

HB_FUNC( GDIMAGECREATE ) /* gdImagePtr gdImageCreate(sx, sy) */
{
   if( HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) )
      hb_retGdImage( gdImageCreate( hb_parni( 1 ) /* sx */,
                                    hb_parni( 2 ) /* sy */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Alias of GDCreate() */
HB_FUNC_TRANSLATE( GDIMAGECREATEPALETTE, GDIMAGECREATE ) /* gdImagePtr gdImageCreatePalette(sx, sy) */

HB_FUNC( GDIMAGECREATETRUECOLOR )                        /* gdImageCreateTrueColor(sx, sy) */
{
   if( HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) )
      hb_retGdImage( gdImageCreateTrueColor( hb_parni( 1 ) /* sx */,
                                             hb_parni( 2 ) /* sy */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* gdImageCreateFromJpegPtr(int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromJpeg( char *szFile ) */
HB_FUNC( GDIMAGECREATEFROMJPEG )
{
   GDImageCreateFrom( IMAGE_JPEG );
}

/* gdImageCreateFromGifPtr(int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromGif( char *szFile ) */
HB_FUNC( GDIMAGECREATEFROMGIF )
{
   GDImageCreateFrom( IMAGE_GIF );
}

/* gdImageCreateFromPngPtr(int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromPng( char *szFile ) */
HB_FUNC( GDIMAGECREATEFROMPNG )
{
   GDImageCreateFrom( IMAGE_PNG );
}

/* gdImagePtr gdImageCreateFromWBMPPtr (int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromWBMP ( char *szFile ) */
HB_FUNC( GDIMAGECREATEFROMWBMP )
{
   GDImageCreateFrom( IMAGE_WBMP );
}

/* gdImagePtr gdImageCreateFromGdPtr (int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromGd ( char *szFile ) */
HB_FUNC( GDIMAGECREATEFROMGD )
{
   GDImageCreateFrom( IMAGE_GD );
}

/* original: void gdImageJpeg(gdImagePtr im, FILE *out) */
/* implementation: void gdImageJpeg(gdImagePtr im, char *szFile) */
HB_FUNC( GDIMAGEJPEG )
{
   GDImageSaveTo( IMAGE_JPEG );
}

/* original: void gdImageGif(gdImagePtr im, FILE *out) */
/* implementation: void gdImageGif(gdImagePtr im, char *szFile) */
HB_FUNC( GDIMAGEGIF )
{
   GDImageSaveTo( IMAGE_GIF );
}

/* original: void gdImagePngEx(gdImagePtr im, FILE *out, int level) */
/* implementation: void gdImagePng(gdImagePtr im, char *szFile [, int level] ) */
HB_FUNC( GDIMAGEPNG )
{
   GDImageSaveTo( IMAGE_PNG );
}

/* original: void gdImageWBmp(gdImagePtr im, FILE *out) */
/* implementation: void gdImageWBmp(gdImagePtr im, char *szFile, int fg) */
HB_FUNC( GDIMAGEWBMP )
{
   GDImageSaveTo( IMAGE_WBMP );
}

/* original: void gdImageGD(gdImagePtr im, FILE *out) */
/* implementation: void gdImageGD(gdImagePtr im, char *szFile) */
HB_FUNC( GDIMAGEGD )
{
   GDImageSaveTo( IMAGE_GD );
}

#if defined( HB_LEGACY_LEVEL4 )

/* After Przemek changes on hb_*ptr() functions, this is a void function held only
   for compatibility with GD library. */
HB_FUNC( GDIMAGEDESTROY ) /* gdImageDestroy(gdImagePtr im) */
{
}

#endif

/* DRAWING FUNCTIONS */

HB_FUNC( GDIMAGESETPIXEL ) /* void gdImageSetPixel(gdImagePtr im, int x, int y, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageSetPixel( im,
                          hb_parni( 2 ) /* x */,
                          hb_parni( 3 ) /* y */,
                          hb_parni( 4 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGELINE ) /* void gdImageLine(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageLine( im,
                      hb_parni( 2 ) /* x1 */,
                      hb_parni( 3 ) /* y1 */,
                      hb_parni( 4 ) /* x2 */,
                      hb_parni( 5 ) /* y2 */,
                      hb_parni( 6 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEDASHEDLINE ) /* void gdImageDashedLine(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageDashedLine( im,
                            hb_parni( 2 ) /* x1 */,
                            hb_parni( 3 ) /* y1 */,
                            hb_parni( 4 ) /* x2 */,
                            hb_parni( 5 ) /* y2 */,
                            hb_parni( 6 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* original: void gdImagePolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
/* implementation: void gdImagePolygon(gdImagePtr im, gdPointPtr points, int color) */
HB_FUNC( GDIMAGEPOLYGON )
{
   if( hb_isGdImage( 1 ) &&
       HB_ISARRAY( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
      {
         PHB_ITEM pPoints     = hb_param( 2, HB_IT_ARRAY );
         int      pointsTotal = ( int ) hb_arrayLen( pPoints );
         int      color       = hb_parni( 3 );

         /* Max Points of polygon */
         gdPoint * points = ( gdPoint * ) hb_xgrab( sizeof( gdPoint ) * pointsTotal );
         int       i;

         for( i = 0; i < pointsTotal; i++ )
         {
            PHB_ITEM pPoint = hb_arrayGetItemPtr( pPoints, i + 1 );
            if( HB_IS_ARRAY( pPoint ) )
            {
               points[ i ].x = hb_arrayGetNI( pPoint, 1 );
               points[ i ].y = hb_arrayGetNI( pPoint, 2 );
            }
            else
            {
               points[ i ].x = 0;
               points[ i ].y = 0;
            }
         }

         /* Draw a polygon */
         gdImagePolygon( im, ( gdPointPtr ) points, pointsTotal, color );

         hb_xfree( points );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* original: void gdImageOpenPolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
/* implementation: void gdImageOpenPolygon(gdImagePtr im, gdPointPtr points, int color) */
HB_FUNC( GDIMAGEOPENPOLYGON )
{
#if HB_GD_VERS( 2, 0, 33 )
   if( hb_isGdImage( 1 ) &&
       HB_ISARRAY( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
      {
         PHB_ITEM pPoints     = hb_param( 2, HB_IT_ARRAY );
         int      pointsTotal = ( int ) hb_arrayLen( pPoints );
         int      color       = hb_parni( 3 );

         /* Max Points of polygon */
         gdPoint * points = ( gdPoint * ) hb_xgrab( sizeof( gdPoint ) * pointsTotal );
         int       i;

         for( i = 0; i < pointsTotal; i++ )
         {
            PHB_ITEM pPoint = hb_arrayGetItemPtr( pPoints, i + 1 );
            if( HB_IS_ARRAY( pPoint ) )
            {
               points[ i ].x = hb_arrayGetNI( pPoint, 1 );
               points[ i ].y = hb_arrayGetNI( pPoint, 2 );
            }
            else
            {
               points[ i ].x = 0;
               points[ i ].y = 0;
            }
         }

         /* Draw a polygon */
         gdImageOpenPolygon( im, ( gdPointPtr ) points, pointsTotal, color );

         hb_xfree( points );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

HB_FUNC( GDIMAGERECTANGLE ) /* void gdImageRectangle(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageRectangle( im,
                           hb_parni( 2 ) /* x1 */,
                           hb_parni( 3 ) /* y1 */,
                           hb_parni( 4 ) /* x2 */,
                           hb_parni( 5 ) /* y2 */,
                           hb_parni( 6 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* original: void gdImageFilledPolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
/* implementation: void gdImageFilledPolygon(gdImagePtr im, gdPointPtr points, int color) */
HB_FUNC( GDIMAGEFILLEDPOLYGON )
{
   if( hb_isGdImage( 1 ) &&
       HB_ISARRAY( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
      {
         PHB_ITEM pPoints     = hb_param( 2, HB_IT_ARRAY );
         int      pointsTotal = ( int ) hb_arrayLen( pPoints );
         int      color       = hb_parni( 3 );

         /* Max Points of polygon */
         gdPoint * points = ( gdPoint * ) hb_xgrab( sizeof( gdPoint ) * pointsTotal );
         int       i;

         for( i = 0; i < pointsTotal; i++ )
         {
            PHB_ITEM pPoint = hb_arrayGetItemPtr( pPoints, i + 1 );
            if( HB_IS_ARRAY( pPoint ) )
            {
               points[ i ].x = hb_arrayGetNI( pPoint, 1 );
               points[ i ].y = hb_arrayGetNI( pPoint, 2 );
            }
            else
            {
               points[ i ].x = 0;
               points[ i ].y = 0;
            }
         }

         /* Draw a filled polygon */
         gdImageFilledPolygon( im, ( gdPointPtr ) points, pointsTotal, color );

         hb_xfree( points );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEFILLEDRECTANGLE ) /* void gdImageFilledRectangle(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageFilledRectangle( im,
                                 hb_parni( 2 ) /* x1 */,
                                 hb_parni( 3 ) /* y1 */,
                                 hb_parni( 4 ) /* x2 */,
                                 hb_parni( 5 ) /* y2 */,
                                 hb_parni( 6 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEARC ) /* void gdImageArc(gdImagePtr im, int cx, int cy, int w, int h, int s, int e, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageArc( im,
                     hb_parni( 2 ) /* cx */,
                     hb_parni( 3 ) /* cy */,
                     hb_parni( 4 ) /* width and height */,
                     hb_parni( 5 ) /* h */,
                     hb_parni( 6 ) /* starting and ending degree */,
                     hb_parni( 7 ) /* e */,
                     hb_parni( 8 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEFILLEDARC ) /* void gdImageFilledArc(gdImagePtr im, int cx, int cy, int w, int h, int s, int e, int color[, int style]) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageFilledArc( im,
                           hb_parni( 2 ) /* cx */,
                           hb_parni( 3 ) /* cy */,
                           hb_parni( 4 ) /* width and height */,
                           hb_parni( 5 ) /* h */,
                           hb_parni( 6 ) /* starting and ending degree */,
                           hb_parni( 7 ) /* e */,
                           hb_parni( 8 ) /* color */,
                           hb_parnidef( 9, gdNoFill ) /* style */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEFILLEDELLIPSE ) /* void gdImageFilledEllipse(gdImagePtr im, int cx, int cy, int w, int h, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageFilledEllipse( im,
                               hb_parni( 2 ) /* cx */,
                               hb_parni( 3 ) /* cy */,
                               hb_parni( 4 ) /* w */,
                               hb_parni( 5 ) /* h */,
                               hb_parni( 6 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEFILLTOBORDER ) /* void gdImageFillToBorder(gdImagePtr im, int x, int y, int border, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageFillToBorder( im,
                              hb_parni( 2 ) /* x */,
                              hb_parni( 3 ) /* y */,
                              hb_parni( 4 ) /* border */,
                              hb_parni( 5 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Disabled, because there is a .prg implementation which
   works with all gd lib versions. [vszakats] */
#if 0
HB_FUNC( GDIMAGEELLIPSE ) /* void gdImageEllipse(gdImagePtr im, int cx, int cy, int w, int h, int color) */
{
#if HB_GD_VERS( 2, 0, 35 )
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageEllipse( im,
                         hb_parni( 2 ) /* cx */,
                         hb_parni( 3 ) /* cy */,
                         hb_parni( 4 ) /* w */,
                         hb_parni( 5 ) /* h */,
                         hb_parni( 6 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}
#endif

HB_FUNC( GDIMAGEFILL ) /* void gdImageFill(gdImagePtr im, int x, int y, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageFill( im,
                      hb_parni( 2 ) /* x */,
                      hb_parni( 3 ) /* y */,
                      hb_parni( 4 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESETANTIALIASED ) /* void gdImageSetAntiAliased(gdImagePtr im, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageSetAntiAliased( im, hb_parni( 2 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESETANTIALIASEDDONTBLEND ) /* void gdImageSetAntiAliasedDontBlend(gdImagePtr im, int c, int dont_blend) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageSetAntiAliasedDontBlend( im,
                                         hb_parni( 2 ) /* color */,
                                         hb_parni( 3 ) /* dont_blend */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESETBRUSH ) /* void gdImageSetBrush(gdImagePtr im, gdImagePtr brush) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) )
   {
      gdImagePtr im    = hb_parGdImage( 1 );
      gdImagePtr brush = hb_parGdImage( 2 );

      if( im && brush )
         gdImageSetBrush( im, brush );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESETTILE ) /* void gdImageSetTile(gdImagePtr im, gdImagePtr tile) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) )
   {
      gdImagePtr im   = hb_parGdImage( 1 );
      gdImagePtr tile = hb_parGdImage( 2 );

      if( im && tile )
         gdImageSetTile( im, tile );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* original: void gdImageSetStyle(gdImagePtr im, int *style, int styleLength) */
/* implementation: void gdImageSetStyle(gdImagePtr im, int *style) */
HB_FUNC( GDIMAGESETSTYLE )
{
   if( hb_isGdImage( 1 ) &&
       HB_ISARRAY( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
      {
         PHB_ITEM pStyles     = hb_param( 2, HB_IT_ARRAY );
         int      styleLength = ( int ) hb_arrayLen( pStyles );

         /* Max numbery of Styles */
         int * styles = ( int * ) hb_xgrab( sizeof( int ) * styleLength );
         int   i;

         for( i = 0; i < styleLength; i++ )
            styles[ i ] = hb_arrayGetNI( pStyles, i + 1 );

         /* Set style */
         gdImageSetStyle( im, ( int * ) styles, styleLength );

         hb_xfree( styles );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESETTHICKNESS ) /* void gdImageSetThickness(gdImagePtr im, int thickness) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
      {
         int thickness = hb_parni( 2 );

         hb_retni( im->thick );  /* Return previous */

         gdImageSetThickness( im, thickness );
      }
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEALPHABLENDING ) /* void gdImageAlphaBlending(gdImagePtr im, int blending) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISLOG( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageAlphaBlending( im, hb_parl( 2 ) ? 1 : 0 /* blending */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESAVEALPHA ) /* void gdImageSaveAlpha(gdImagePtr im, int saveFlag) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISLOG( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageSaveAlpha( im, hb_parl( 2 ) ? 1 : 0 /* saveFlag */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESETCLIP ) /* void gdImageSetClip(gdImagePtr im, int x1, int y1, int x2, int y2) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageSetClip( im,
                         hb_parni( 2 ) /* x1 */,
                         hb_parni( 3 ) /* y1 */,
                         hb_parni( 4 ) /* x2 */,
                         hb_parni( 5 ) /* y2 */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* original: void gdImageGetClip(gdImagePtr im, int *x1P, int *y1P, int *x2P, int *y2P) */
/* implementation: array gdImageGetClip(gdImagePtr im) */
HB_FUNC( GDIMAGEGETCLIP )
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );
      int        x1, y1, x2, y2;
      PHB_ITEM   pClipArray;

      if( im )
         gdImageGetClip( im, &x1, &y1, &x2, &y2 );  /* Get clipping rectangle */
      else
         x1 = y1 = x2 = y2 = 0;

      /* Return clipping rectangle value in an array */
      pClipArray = hb_itemArrayNew( 4 );
      hb_itemPutNI( hb_arrayGetItemPtr( pClipArray, 1 ), x1 );
      hb_itemPutNI( hb_arrayGetItemPtr( pClipArray, 2 ), y1 );
      hb_itemPutNI( hb_arrayGetItemPtr( pClipArray, 3 ), x2 );
      hb_itemPutNI( hb_arrayGetItemPtr( pClipArray, 4 ), y2 );

      /* return array */
      hb_itemReturnRelease( pClipArray );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* QUERY FUNCTIONS */

HB_FUNC( GDIMAGECOLORSTOTAL ) /* int gdImageColorsTotal(gdImagePtr im) */
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retni( im ? gdImageColorsTotal( im ) : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEALPHA ) /* int gdImageAlpha(gdImagePtr im, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retni( im ? gdImageAlpha( im, hb_parni( 2 ) /* color */ ) : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGERED ) /* int gdImageRed(gdImagePtr im, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retni( im ? gdImageRed( im, hb_parni( 2 ) /* color */ ) : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEGREEN ) /* int gdImageGreen(gdImagePtr im, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retni( im ? gdImageGreen( im, hb_parni( 2 ) /* color */ ) : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEBLUE ) /* int gdImageBlue(gdImagePtr im, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retni( im ? gdImageBlue( im, hb_parni( 2 ) /* color */ ) : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESX ) /* int gdImageSX(gdImagePtr im) */
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retni( im ? gdImageSX( im ) : 0 );  /* Get Image Width in pixels */
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESY ) /* int gdImageSX(gdImagePtr im) */
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retni( im ? gdImageSY( im ) : 0 );  /* Get Image Height in pixels */
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEGETPIXEL ) /* int gdImageGetPixel(gdImagePtr im, int x, int y) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      /* Get Color of a pixel */
      if( im )
         hb_retni( gdImageGetPixel( im,
                                    hb_parni( 2 ) /* x */,
                                    hb_parni( 3 ) /* y */ ) );
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEBOUNDSSAFE ) /* int gdImageBoundsSafe(gdImagePtr im, int x, int y) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      /* Get if pixel in Clipping region */
      hb_retl( im && gdImageBoundsSafe( im,
                                        hb_parni( 2 ) /* x */,
                                        hb_parni( 3 ) /* y */ ) != 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEGETINTERLACED ) /* int gdImageGetInterlaced(gdImagePtr im) */
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retl( im && gdImageGetInterlaced( im ) != 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEGETTRANSPARENT ) /* int gdImageGetTransparent(gdImagePtr im) */
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retl( im && gdImageGetTransparent( im ) != 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGETRUECOLOR ) /* int gdImageTrueColor(gdImagePtr im) */
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retl( im && gdImageTrueColor( im ) != 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGETRUECOLORTOPALETTE ) /* void gdImageTrueColorToPalette (gdImagePtr im, int ditherFlag, int colorsWanted) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISLOG( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      /* Converts a truecolor image to a palette-based image */
      if( im )
         gdImageTrueColorToPalette( im,
                                    hb_parl( 2 ) ? 1 : 0 /* ditherFlag */,
                                    hb_parni( 3 ) /* colorsWanted */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECREATEPALETTEFROMTRUECOLOR ) /* gdImagePtr gdImageCreatePaletteFromTrueColor(gdImagePtr im, int ditherFlag, int colorsWanted) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISLOG( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      /* Converts a truecolor image to a palette-based image and return the image */
      if( im )
         hb_retGdImage( gdImageCreatePaletteFromTrueColor( im,
                                                           hb_parl( 2 ) ? 1 : 0 /* ditherFlag */,
                                                           hb_parni( 3 ) /* colorsWanted */ ) );
      else
         hb_retGdImage( NULL );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEPALETTEPIXEL ) /* int gdImagePalettePixel(gdImagePtr im, int x, int y) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImagePalettePixel( im,
                                        hb_parni( 2 ) /* x */,
                                        hb_parni( 3 ) /* y */ ) );  /* Get Color of a pixel */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGETRUECOLORPIXEL ) /* int gdImageTrueColorPixel(gdImagePtr im, int x, int y) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageTrueColorPixel( im,
                                          hb_parni( 2 ) /* x */,
                                          hb_parni( 3 ) /* y */ ) );  /* Get Color of a pixel */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEGETTHICKNESS ) /* void gdImageGetThickness(gdImagePtr im) */
{
   if( hb_isGdImage( 1 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retni( im ? im->thick : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* FONTS AND TEXT-HANDLING FUNCTIONS */

HB_FUNC( GDFONTGETSMALL ) /* gdFontPtr gdFontGetSmall(void) */
{
   hb_retGdFont( gdFontGetSmall() );
}

HB_FUNC( GDFONTGETLARGE ) /* gdFontPtr gdFontGetLarge(void) */
{
   hb_retGdFont( gdFontGetLarge() );
}

HB_FUNC( GDFONTGETMEDIUMBOLD ) /* gdFontPtr gdFontGetMediumBold(void) */
{
   hb_retGdFont( gdFontGetMediumBold() );
}

HB_FUNC( GDFONTGETGIANT ) /* gdFontPtr gdFontGetGiant(void) */
{
   hb_retGdFont( gdFontGetGiant() );
}

HB_FUNC( GDFONTGETTINY ) /* gdFontPtr gdFontGetTiny(void) */
{
   hb_retGdFont( gdFontGetTiny() );
}

/* void gdImageChar(gdImagePtr im, gdFontPtr font, int x, int y, int c, int color) */
/* void gdImageString(gdImagePtr im, gdFontPtr font, int x, int y, unsigned char *s, int color) */
HB_FUNC( GDIMAGESTRING )
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdFont( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISCHAR( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      gdImagePtr im   = hb_parGdImage( 1 );
      gdFontPtr  font = hb_parGdFont( 2 );

      if( im && font )
         gdImageString( im,
                        font,
                        hb_parni( 3 ) /* x */,
                        hb_parni( 4 ) /* y */,
                        ( unsigned char * ) HB_UNCONST( hb_parc( 5 ) ),
                        hb_parni( 6 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC_TRANSLATE( GDIMAGECHAR, GDIMAGESTRING )

/* void gdImageCharUp(gdImagePtr im, gdFontPtr font, int x, int y, int c, int color) */
/* void gdImageStringUp(gdImagePtr im, gdFontPtr font, int x, int y, unsigned char *s, int color) */
HB_FUNC( GDIMAGESTRINGUP )
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdFont( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISCHAR( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      gdImagePtr im   = hb_parGdImage( 1 );
      gdFontPtr  font = hb_parGdFont( 2 );

      if( im && font )
         gdImageStringUp( im,
                          font,
                          hb_parni( 3 ) /* x */,
                          hb_parni( 4 ) /* y */,
                          ( unsigned char * ) HB_UNCONST( hb_parc( 5 ) ),
                          hb_parni( 6 ) /* color */ );  /* Write string */
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC_TRANSLATE( GDIMAGECHARUP, GDIMAGESTRINGUP )

/* char *gdImageStringFTEx(gdImagePtr im, int *brect, int fg, char *fontname, double ptsize, double angle, int x, int y, char *string, gdFTStringExtraPtr strex) */
/* implementation: cError := gdImageStringFTEx( im, aRect, int fg, cFontname, nPtSize, nAngle, x, y, cString, nLinespacing, nCharmap, nResolution ) */
HB_FUNC( GDIMAGESTRINGFTEX )
{
   if( ( HB_ISNIL( 1 ) || hb_isGdImage( 1 ) ) &&
       HB_ISARRAY( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISCHAR( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISCHAR( 9 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      PHB_ITEM     pRect    = hb_param( 2, HB_IT_ARRAY );
      int          fgcolor  = hb_parni( 3 ); /* foreground color */
      const char * fontname = hb_parc( 4 );
      double       ptsize   = hb_parnd( 5 ); /* point size */
      double       angle    = hb_parnd( 6 ); /* angle value in radians */
      int          x        = hb_parni( 7 );
      int          y        = hb_parni( 8 );

      gdFTStringExtra extra;
      int flags = 0; /* Extended flags */

      /* defaults */
      double linespacing = 1.05;
      int    charmap     = gdFTEX_Unicode;
      int    resolution  = 96;

      int    aRect[ 8 ];
      int    i;
      char * err;

      void * hText;

      /* Retrieve rectangle array */
      for( i = 0; i < 8; i++ )
         aRect[ i ] = hb_arrayGetNI( pRect, i + 1 );

      /* Retrieve line spacing */
      if( HB_ISNUM( 10 ) )
      {
         linespacing = hb_parnd( 10 );
         flags      |= gdFTEX_LINESPACE;
      }

      /* Retrieve charmap */
      if( HB_ISNUM( 11 ) )
      {
         charmap = hb_parni( 11 );
         flags  |= gdFTEX_CHARMAP;
      }

      /* Retrieve resolution */
      if( HB_ISNUM( 12 ) )
      {
         resolution = hb_parni( 12 );
         flags     |= gdFTEX_RESOLUTION;
      }

      if( flags != 0 )
      {
         extra.flags       = flags;
         extra.linespacing = ( flags & gdFTEX_LINESPACE ) ? linespacing : 1.05;
         extra.charmap     = ( flags & gdFTEX_CHARMAP ) ? charmap : gdFTEX_Unicode;
         extra.hdpi        = ( flags & gdFTEX_RESOLUTION ) ? resolution : 96;
         extra.vdpi        = ( flags & gdFTEX_RESOLUTION ) ? resolution : 96;
      }

      /* Write string */
      err = gdImageStringFTEx( im, &aRect[ 0 ], fgcolor, ( char * ) HB_UNCONST( fontname ),
                               ptsize, angle, x, y, ( char * ) HB_UNCONST( hb_parstr_utf8( 9, &hText, NULL ) ),
                               flags != 0 ? &extra : NULL );

      hb_strfree( hText );

      if( ! err )
      {
         /* Save in array the correct text rectangle dimensions */
         PHB_ITEM pArray = hb_itemArrayNew( 8 );
         for( i = 0; i < 8; i++ )
            hb_itemPutNI( hb_arrayGetItemPtr( pArray, i + 1 ), aRect[ i ] );
         hb_itemCopy( pRect, pArray );
         hb_itemRelease( pArray );
      }
      hb_retc( err );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESTRINGFTCIRCLE ) /* char *gdImageStringFTCircle(gdImagePtr im, int cx, int cy, double radius, double textRadius, double fillPortion, char *font, double points, char *top, char *bottom, int fgcolor) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISCHAR( 7 ) &&
       HB_ISNUM( 8 ) &&
       ( HB_ISNIL( 9 ) || HB_ISCHAR( 9 ) ) &&
       ( HB_ISNIL( 10 ) || HB_ISCHAR( 10 ) ) &&
       HB_ISNUM( 11 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
      {
         void * hTop    = NULL;
         void * hBottom = NULL;

         hb_retc( gdImageStringFTCircle( im,
                                         hb_parni( 2 ) /* cx */,
                                         hb_parni( 3 ) /* cy */,
                                         hb_parnd( 4 ) /* radius */,
                                         hb_parnd( 5 ) /* textRadius */,
                                         hb_parnd( 6 ) /* fillPortion */,
                                         ( char * ) HB_UNCONST( hb_parc( 7 ) ) /* font name */,
                                         hb_parnd( 8 ) /* points */,
                                         ( char * ) HB_UNCONST( HB_ISCHAR( 9 ) ? hb_parstr_utf8( 9, &hTop, NULL ) : "" ),
                                         ( char * ) HB_UNCONST( HB_ISCHAR( 10 ) ? hb_parstr_utf8( 10, &hBottom, NULL ) : "" ),
                                         hb_parni( 11 ) /* foreground color */ ) );

         hb_strfree( hTop );
         hb_strfree( hBottom );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDFONTCACHESETUP ) /* int gdFontCacheSetup (void) */
{
   /* This function initializes the font cache for freetype text output functions */
   hb_retl( gdFontCacheSetup() != 0 );
}

HB_FUNC( GDFONTCACHESHUTDOWN ) /* void gdFontCacheShutdown (void) */
{
   /* This function initializes the font cache for freetype text output functions */
   gdFontCacheShutdown();
}

HB_FUNC( GDFONTGETWIDTH )
{
   if( hb_isGdFont( 1 ) )
   {
      gdFontPtr font = hb_parGdFont( 1 );

      hb_retni( font ? font->w : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDFONTGETHEIGHT )
{
   if( hb_isGdFont( 1 ) )
   {
      gdFontPtr font = hb_parGdFont( 1 );

      hb_retni( font ? font->h : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* COLOR HANDLING FUNCTIONS */

HB_FUNC( GDIMAGECOLORALLOCATE ) /* int gdImageColorAllocate(gdImagePtr im, int r, int g, int b) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageColorAllocate( im,
                                         hb_parni( 2 ) /* R */,
                                         hb_parni( 3 ) /* G */,
                                         hb_parni( 4 ) /* B */ ) );  /* return color */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLORDEALLOCATE ) /* void gdImageColorDeallocate(gdImagePtr im, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageColorDeallocate( im, hb_parni( 2 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLORALLOCATEALPHA ) /* int gdImageColorAllocateAlpha(gdImagePtr im, int r, int g, int b, int a) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageColorAllocateAlpha( im,
                                              hb_parni( 2 ) /* R */,
                                              hb_parni( 3 ) /* G */,
                                              hb_parni( 4 ) /* B */,
                                              hb_parni( 5 ) /* alpha */ ) );  /* return color */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLORCLOSEST ) /* int gdImageColorClosest(gdImagePtr im, int r, int g, int b) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageColorClosest( im,
                                        hb_parni( 2 ) /* R */,
                                        hb_parni( 3 ) /* G */,
                                        hb_parni( 4 ) /* B */ ) );  /* return color */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLORCLOSESTALPHA ) /* int gdImageColorClosestAlpha(gdImagePtr im, int r, int g, int b, int a) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageColorClosestAlpha( im,
                                             hb_parni( 2 ) /* R */,
                                             hb_parni( 3 ) /* G */,
                                             hb_parni( 4 ) /* B */,
                                             hb_parni( 5 ) /* A */ ) );  /* return color */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLORCLOSESTHWB ) /*  gdImageColorClosestHWB(gdImagePtr im, int r, int g, int b) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageColorClosestHWB( im,
                                           hb_parni( 2 ) /* R */,
                                           hb_parni( 3 ) /* G */,
                                           hb_parni( 4 ) /* B */ ) );  /* return color */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLOREXACT ) /* int gdImageColorExact(gdImagePtr im, int r, int g, int b) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageColorExact( im,
                                      hb_parni( 2 ) /* R */,
                                      hb_parni( 3 ) /* G */,
                                      hb_parni( 4 ) /* B */ ) );  /* return color */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLORRESOLVE ) /* int gdImageColorResolve(gdImagePtr im, int r, int g, int b) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageColorResolve( im,
                                        hb_parni( 2 ) /* R */,
                                        hb_parni( 3 ) /* G */,
                                        hb_parni( 4 ) /* B */ ) );  /* return color */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLORRESOLVEALPHA ) /* int gdImageColorResolveAlpha(gdImagePtr im, int r, int g, int b, int a) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         hb_retni( gdImageColorResolveAlpha( im,
                                             hb_parni( 2 ) /* R */,
                                             hb_parni( 3 ) /* G */,
                                             hb_parni( 4 ) /* B */,
                                             hb_parni( 5 ) /* A */ ) );  /* return color */
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOLORTRANSPARENT ) /* void gdImageColorTransparent(gdImagePtr im, int color) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      /* Set transparent color (to define no transparent color set -1) */
      if( im )
         gdImageColorTransparent( im, hb_parni( 2 ) /* color */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDTRUECOLOR ) /* int gdTrueColor(int red, int green, int blue) */
{
   if( HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) )
      hb_retni( gdTrueColor( hb_parni( 1 ) /* R */,
                             hb_parni( 2 ) /* G */,
                             hb_parni( 3 ) /* B */ ) );  /* return color */
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDTRUECOLORALPHA ) /* int gdTrueColorAlpha(int red, int green, int blue, int alpha) */
{
   if( HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
      hb_retni( gdTrueColorAlpha( hb_parni( 1 ) /* R */,
                                  hb_parni( 2 ) /* G */,
                                  hb_parni( 3 ) /* B */,
                                  hb_parni( 4 ) /* A */ ) );  /* return color */
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* COPY AND RESIZING FUNCTIONS */

HB_FUNC( GDIMAGECOPY ) /* void gdImageCopy(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) )
   {
      gdImagePtr dst = hb_parGdImage( 1 );
      gdImagePtr src = hb_parGdImage( 2 );

      if( dst && src )
         gdImageCopy( dst, src,
                      hb_parni( 3 ) /* dstX */,
                      hb_parni( 4 ) /* dstY */,
                      hb_parni( 5 ) /* srcX */,
                      hb_parni( 6 ) /* srcY */,
                      hb_parni( 7 ) /* w */,
                      hb_parni( 8 ) /* h */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOPYRESIZED ) /* void gdImageCopyResized(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int dstW, int dstH, int srcW, int srcH) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 ) &&
       HB_ISNUM( 10 ) )
   {
      gdImagePtr dst = hb_parGdImage( 1 );
      gdImagePtr src = hb_parGdImage( 2 );

      if( dst && src )
         gdImageCopyResized( dst, src,
                             hb_parni( 3 ) /* dstX */,
                             hb_parni( 4 ) /* dstY */,
                             hb_parni( 5 ) /* srcX */,
                             hb_parni( 6 ) /* srcY */,
                             hb_parni( 7 ) /* dstW */,
                             hb_parni( 8 ) /* dstH */,
                             hb_parni( 9 ) /* srcW */,
                             hb_parni( 10 ) /* srcH */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOPYRESAMPLED ) /* void gdImageCopyResampled(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int dstW, int dstH, int srcW, int srcH) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 ) &&
       HB_ISNUM( 10 ) )
   {
      gdImagePtr dst = hb_parGdImage( 1 );
      gdImagePtr src = hb_parGdImage( 2 );

      if( dst && src )
         gdImageCopyResampled( dst, src,
                               hb_parni( 3 ) /* dstX */,
                               hb_parni( 4 ) /* dstY */,
                               hb_parni( 5 ) /* srcX */,
                               hb_parni( 6 ) /* srcY */,
                               hb_parni( 7 ) /* dstW */,
                               hb_parni( 8 ) /* dstH */,
                               hb_parni( 9 ) /* srcW */,
                               hb_parni( 10 ) /* srcH */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOPYROTATED ) /* void gdImageCopyRotated(gdImagePtr dst, gdImagePtr src, double dstX, double dstY, int srcX, int srcY, int srcW, int srcH, int angle) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 ) )
   {
      gdImagePtr dst = hb_parGdImage( 1 );
      gdImagePtr src = hb_parGdImage( 2 );

      if( dst && src )
         gdImageCopyRotated( dst, src,
                             hb_parnd( 3 ) /* dstX */,
                             hb_parnd( 4 ) /* dstY */,
                             hb_parni( 5 ) /* srcX */,
                             hb_parni( 6 ) /* srcY */,
                             hb_parni( 7 ) /* srcW */,
                             hb_parni( 8 ) /* srcH */,
                             hb_parni( 9 ) /* angle */ );  /* Perform rotation */
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOPYMERGE ) /* void gdImageCopyMerge(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h, int pct) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 ) )
   {
      gdImagePtr dst = hb_parGdImage( 1 );
      gdImagePtr src = hb_parGdImage( 2 );

      if( dst && src )
         gdImageCopyMerge( dst, src,
                           hb_parni( 3 ) /* dstX */,
                           hb_parni( 4 ) /* dstY */,
                           hb_parni( 5 ) /* srcX */,
                           hb_parni( 6 ) /* srcY */,
                           hb_parni( 7 ) /* w */,
                           hb_parni( 8 ) /* h */,
                           hb_parni( 9 ) /* pct */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGECOPYMERGEGRAY ) /* void gdImageCopyMergeGray(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h, int pct) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 ) )
   {
      gdImagePtr dst = hb_parGdImage( 1 );
      gdImagePtr src = hb_parGdImage( 2 );

      if( dst && src )
         gdImageCopyMergeGray( dst, src,
                               hb_parni( 3 ) /* dstX */,
                               hb_parni( 4 ) /* dstY */,
                               hb_parni( 5 ) /* srcX */,
                               hb_parni( 6 ) /* srcY */,
                               hb_parni( 7 ) /* w */,
                               hb_parni( 8 ) /* h */,
                               hb_parni( 9 ) /* pct */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEPALETTECOPY ) /* void gdImagePaletteCopy(gdImagePtr dst, gdImagePtr src) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) )
   {
      gdImagePtr dst = hb_parGdImage( 1 );
      gdImagePtr src = hb_parGdImage( 2 );

      if( dst && src )
         gdImagePaletteCopy( dst, src );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESQUARETOCIRCLE ) /* void gdImageSquareToCircle(gdImagePtr im, int radius) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      hb_retGdImage( im ? gdImageSquareToCircle( im, hb_parni( 2 ) /* radius */ ) : NULL );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGESHARPEN ) /* void gdImageSharpen(gdImagePtr im, int pct) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageSharpen( im, hb_parni( 2 ) /* pct */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* MISCELLANEOUS FUNCTIONS */

HB_FUNC( GDIMAGECOMPARE ) /* int gdImageCompare(gdImagePtr im1, gdImagePtr im2) */
{
   if( hb_isGdImage( 1 ) &&
       hb_isGdImage( 2 ) )
   {
      gdImagePtr im1 = hb_parGdImage( 1 );
      gdImagePtr im2 = hb_parGdImage( 2 );

      /* Compare images - if return != 0 check value for infos */
      if( im1 && im2 )
         hb_retni( gdImageCompare( im1, im2 ) );
      else
         hb_retni( -1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( GDIMAGEINTERLACE ) /* void gdImageInterlace(gdImagePtr im, int interlace) */
{
   if( hb_isGdImage( 1 ) &&
       HB_ISLOG( 2 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
         gdImageInterlace( im, hb_parl( 2 ) ? 1 : 0 /* interlace */ );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if HB_GD_VERS( 2, 0, 33 )

static void AddImageToFile( const char * szFile, const void * iptr, int sz )
{
   PHB_FILE fhandle = hb_fileExtOpen( szFile, NULL,
                                      FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE |
                                      FXO_APPEND | FXO_SHARELOCK,
                                      NULL, NULL );

   if( fhandle )
   {
      hb_fileSeek( fhandle, 0, FS_END );
      hb_fileWrite( fhandle, iptr, ( HB_SIZE ) sz, -1 );
      hb_fileClose( fhandle );
   }
}

#endif

/*BGD_DECLARE(void *) gdImageGifAnimBeginPtr(gdImagePtr im, int *size, int GlobalCM, int Loops); */
/* implementation: (void *) gdImageGifAnimBegin( gdImagePtr im, cFile | nHandle, int GlobalCM, int Loops); */
HB_FUNC( GDIMAGEGIFANIMBEGIN )
{
#if HB_GD_VERS( 2, 0, 33 )
   if( hb_isGdImage( 1 ) &&
       ( HB_ISCHAR( 2 ) || hb_fileParamGet( 2 ) || HB_ISNUM( 2 ) || HB_ISNIL( 2 ) ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
      {
         int    size;
         void * iptr = gdImageGifAnimBeginPtr( im,
                                               &size,
                                               hb_parni( 3 ) /* global color map */,
                                               hb_parni( 4 ) /* loops */ );

         /* Check if parameter is a file name or a handle */
         if( HB_ISCHAR( 2 ) )
            SaveImageToFile( hb_parc( 2 ), iptr, size );
         else if( hb_fileParamGet( 2 ) )
            SaveImageToFileObject( hb_fileParamGet( 2 ), iptr, size );
         else
            SaveImageToHandle( hb_numToHandle( hb_parnintdef( 2, HB_STDOUT_HANDLE ) ), iptr, size );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

/* BGD_DECLARE(void *) gdImageGifAnimAddPtr(gdImagePtr im, int *size, int LocalCM, int LeftOfs, int TopOfs, int Delay, int Disposal, gdImagePtr previm); */
/* implementation: (void *) gdImageGifAnimAdd( gdImagePtr im, cFile | nHandle, int LocalCM, int LeftOfs, int TopOfs, int Delay, int Disposal, gdImagePtr previm); */
HB_FUNC( GDIMAGEGIFANIMADD )
{
#if HB_GD_VERS( 2, 0, 33 )
   if( hb_isGdImage( 1 ) &&
       ( HB_ISCHAR( 2 ) || hb_fileParamGet( 2 ) || HB_ISNUM( 2 ) || HB_ISNIL( 2 ) ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       ( hb_isGdImage( 8 ) || HB_ISNIL( 8 ) ) )
   {
      gdImagePtr im = hb_parGdImage( 1 );

      if( im )
      {
         int    size;
         void * iptr = gdImageGifAnimAddPtr( im,
                                             &size,
                                             hb_parni( 3 ) /* LocalCM */,
                                             hb_parni( 4 ) /* LeftOfs */,
                                             hb_parni( 5 ) /* TopOfs */,
                                             hb_parni( 6 ) /* Delay */,
                                             hb_parni( 7 ) /* Disposal */,
                                             hb_parGdImage( 8 ) /* previm */ );

         /* Check if parameter is a file name or a handle */
         if( HB_ISCHAR( 2 ) )
            AddImageToFile( hb_parc( 2 ), iptr, size );
         else if( hb_fileParamGet( 2 ) )
            SaveImageToFileObject( hb_fileParamGet( 2 ), iptr, size );
         else
            SaveImageToHandle( hb_numToHandle( hb_parnintdef( 2, HB_STDOUT_HANDLE ) ), iptr, size );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

/* BGD_DECLARE(void *) gdImageGifAnimEndPtr(int *size); */
/* implementation: gdImageGifAnimEnd( cFile | nHandle ); */
HB_FUNC( GDIMAGEGIFANIMEND )
{
#if HB_GD_VERS( 2, 0, 33 )
   if( HB_ISCHAR( 1 ) || hb_fileParamGet( 1 ) || HB_ISNUM( 1 ) || HB_ISNIL( 1 ) )
   {
      int    size;
      void * iptr = gdImageGifAnimEndPtr( &size );

      /* Check if 1st parameter is a file name or a handle */
      if( HB_ISCHAR( 1 ) )
         AddImageToFile( hb_parc( 1 ), iptr, size );
      else if( hb_fileParamGet( 1 ) )
         SaveImageToFileObject( hb_fileParamGet( 1 ), iptr, size );
      else
         SaveImageToHandle( hb_numToHandle( hb_parnintdef( 1, HB_STDOUT_HANDLE ) ), iptr, size );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}
