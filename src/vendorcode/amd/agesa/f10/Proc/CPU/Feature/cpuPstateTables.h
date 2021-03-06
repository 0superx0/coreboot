/**
 * @file
 *
 * AMD AGESA CPU Pstate Table Functions declarations.
 *
 * Contains code that declares the AGESA CPU _PSS related APIs
 *
 * @xrefitem bom "File Content Label" "Release Content"
 * @e project:      AGESA
 * @e sub-project:  CPU/Feature
 * @e \$Revision: 13766 $   @e \$Date: 2009-05-14 07:22:59 +0800 (Thu, 14 May 2009) $
 *
 */
/*
 ******************************************************************************
 *
 * Copyright (c) 2011, Advanced Micro Devices, Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Advanced Micro Devices, Inc. nor the names of 
 *       its contributors may be used to endorse or promote products derived 
 *       from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL ADVANCED MICRO DEVICES, INC. BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 ******************************************************************************
 */

#ifndef _CPU_PSTATE_TABLES_H_
#define _CPU_PSTATE_TABLES_H_

/*----------------------------------------------------------------------------------------
 *          M I X E D   (Definitions And Macros / Typedefs, Structures, Enums)
 *----------------------------------------------------------------------------------------
 */
// Forward declaration.
typedef struct _PSTATE_CPU_FAMILY_SERVICES PSTATE_CPU_FAMILY_SERVICES;

/*----------------------------------------------------------------------------------------
 *                 D E F I N I T I O N S     A N D     M A C R O S
 *----------------------------------------------------------------------------------------
 */

/*----------------------------------------------------------------------------------------
 *                    T Y P E D E F S     A N D     S T R U C T U R E S
 *----------------------------------------------------------------------------------------
 */
/*---------------------------------------------------------------------------------------*/
/**
 *  Family specific call to check if Pstate PSD is dependent.
 *
 * @param[in]     PstateCpuFamilyServices  Pstate CPU services.
 * @param[in,out] PlatformConfig     Contains the runtime modifiable feature input data.
 * @param[in]     StdHeader          Config Handle for library, services.
 *
 * @retval       TRUE               PSD is dependent.
 * @retval       FALSE              PSD is independent.
 *
 */
typedef BOOLEAN F_PSTATE_PSD_IS_DEPENDENT (
  IN       PSTATE_CPU_FAMILY_SERVICES *PstateCpuServices,
  IN OUT   PLATFORM_CONFIGURATION *PlatformConfig,
  IN       AMD_CONFIG_PARAMS *StdHeader
  );

/// Reference to a Method.
typedef F_PSTATE_PSD_IS_DEPENDENT *PF_PSTATE_PSD_IS_DEPENDENT;

/**
 *  Family specific call to set core TscFreqSel.
 *
 * @param[in]     PstateCpuFamilyServices  Pstate CPU services.
 * @param[in]     StdHeader          Config Handle for library, services.
 *
 */
typedef VOID F_PSTATE_SET_TSC_FREQ_SEL (
  IN       PSTATE_CPU_FAMILY_SERVICES *PstateCpuServices,
  IN       AMD_CONFIG_PARAMS *StdHeader
  );

/// Reference to a Method.
typedef F_PSTATE_SET_TSC_FREQ_SEL *PF_PSTATE_SET_TSC_FREQ_SEL;

/**
 * Provide the interface to the PSD dependent Family Specific Services.
 *
 * Use the methods or data in this struct to adapt the feature code to a specific cpu family or model (or stepping!).
 * Each supported Family must provide an implementation for all methods in this interface, even if the
 * implementation is a CommonReturn().
 */
struct _PSTATE_CPU_FAMILY_SERVICES {
  UINT16          Revision;                                         ///< Interface version
  // Public Methods.
  PF_PSTATE_PSD_IS_DEPENDENT IsPstatePsdDependent;                  ///< Method: Family specific call to check if PSD is dependent.
  PF_PSTATE_SET_TSC_FREQ_SEL CpuSetTscFreqSel;                      ///< Method: Family specific call to set core TscFreqSel.
};


/*----------------------------------------------------------------------------------------
 *                          F U N C T I O N S     P R O T O T Y P E
 *----------------------------------------------------------------------------------------
 */

#endif  // _CPU_PSTATE_TABLES_H_
