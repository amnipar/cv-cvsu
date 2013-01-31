#include <bindings.dsl.h>
#include "cvsu_opencv.h"

module CV.Bindings.CVSU where

#strict_import

import CVSU.C.Types

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#opaque_t IplImage

type CIplImage = C'IplImage

#ccall pixel_image_create_from_ipl_image , \
  Ptr CPixelImage -> Ptr <IplImage> -> CPixelFormat -> IO CResult

#ccall ipl_image_create_from_pixel_image , \
  Ptr (Ptr <IplImage>) -> Ptr CPixelImage -> CPixelFormat -> IO CResult

#ccall pixel_image_create_from_file , \
  Ptr CPixelImage -> CString -> CPixelFormat -> CPixelType -> IO CResult

#ccall pixel_image_write_to_file , \
  Ptr CPixelImage -> CString -> IO CResult
