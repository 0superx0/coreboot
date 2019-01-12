package main

type haswellmc struct {
	variant string
}

func (i haswellmc) Scan(ctx Context, addr PCIDevData) {
	inteltool := ctx.InfoSource.GetInteltool()

	/* FIXME:XX Move this somewhere else.  */
	MainboardIncludes = append(MainboardIncludes, "drivers/intel/gma/int15.h")
	MainboardEnable += (`	/* FIXME: fix those values*/
	install_intel_vga_int15_handler(GMA_INT15_ACTIVE_LFP_INT_LVDS, GMA_INT15_PANEL_FIT_DEFAULT, GMA_INT15_BOOT_DISPLAY_DEFAULT, 0);
`)

	pchLVDS := inteltool.IGD[0xe1180]
	dualChannel := pchLVDS&(3<<2) == (3 << 2)
	pipe := (pchLVDS >> 30) & 1
	link_m1 := inteltool.IGD[0x60040+0x1000*pipe]
	link_n1 := inteltool.IGD[0x60044+0x1000*pipe]
	link_factor := float32(link_m1) / float32(link_n1)
	fp0 := inteltool.IGD[0xc6040+8*pipe]
	dpll := inteltool.IGD[0xc6014+4*pipe]
	pixel_m2 := fp0 & 0xff
	pixel_m1 := (fp0>>8)&0xff + 2
	pixel_p1 := uint32(1)
	for i := dpll & 0x1ffff; i != 0 && i&1 == 0; i >>= 1 {
		pixel_p1++
	}
	pixel_n := ((fp0 >> 16) & 0xff) + 2
	pixel_frequency := float32(120000*(5*pixel_m1+pixel_m2)) / float32(pixel_n*pixel_p1*7.0)
	if !dualChannel {
		pixel_frequency /= 2
	}
	link_frequency := pixel_frequency / link_factor
	DevTree = DevTreeNode{
		Chip:          "northbridge/intel/haswell",
		MissingParent: "northbridge",
		Comment:       "FIXME: check gfx.ndid and gfx.did",
		Registers: map[string]string{
			"gpu_dp_b_hotplug":                    FormatInt32((inteltool.IGD[0xc4030] >> 2) & 7),
			"gpu_dp_c_hotplug":                    FormatInt32((inteltool.IGD[0xc4030] >> 10) & 7),
			"gpu_dp_d_hotplug":                    FormatInt32((inteltool.IGD[0xc4030] >> 18) & 7),
			"gpu_panel_port_select":               FormatInt32((inteltool.IGD[0xc7208] >> 30) & 3),
			"gpu_panel_power_up_delay":            FormatInt32((inteltool.IGD[0xc7208] >> 16) & 0x1fff),
			"gpu_panel_power_backlight_on_delay":  FormatInt32(inteltool.IGD[0xc7208] & 0x1fff),
			"gpu_panel_power_down_delay":          FormatInt32((inteltool.IGD[0xc720c] >> 16) & 0x1fff),
			"gpu_panel_power_backlight_off_delay": FormatInt32(inteltool.IGD[0xc720c] & 0x1fff),
			"gpu_panel_power_cycle_delay":         FormatInt32(inteltool.IGD[0xc7210] & 0xff),
			"gpu_cpu_backlight":                   FormatHex32(inteltool.IGD[0x48254]),
			"gpu_pch_backlight":                   FormatHex32((inteltool.IGD[0xc8254] >> 16) * 0x10001),
			"gfx.use_spread_spectrum_clock":       FormatBool((inteltool.IGD[0xc6200]>>12)&1 != 0),
			"gfx.link_frequency_270_mhz":          FormatBool(link_frequency > 200000),
			/* FIXME:XX hardcoded.  */
			"gfx.ndid": "3",
			"gfx.did":  "{ 0x80000100, 0x80000240, 0x80000410, 0x80000410, 0x00000005 }",
		},
		Children: []DevTreeNode{
			{
				Chip: "cpu_cluster",
				Dev:  0,
				Children: []DevTreeNode{
					{
						Chip: "cpu/intel/socket_rPGA989",
						Children: []DevTreeNode{
							{
								Chip: "lapic",
								Dev:  0,
							},
						},
					},

					{
						Chip:    "cpu/intel/model_206ax",
						Comment: "FIXME: check all registers",
						Registers: map[string]string{
							/* FIXME:XX hardcoded.  */
							"c1_acpower": "1",
							"c2_acpower": "3",
							"c3_acpower": "5",
							"c1_battery": "1",
							"c2_battery": "3",
							"c3_battery": "5",
						},
						Children: []DevTreeNode{
							{
								Chip:     "lapic",
								Dev:      0xacac,
								Disabled: true,
							},
						},
					},
				},
			},

			{
				Chip:          "domain",
				Dev:           0,
				PCIController: true,
				ChildPCIBus:   0,
				PCISlots: []PCISlot{
					PCISlot{PCIAddr: PCIAddr{Dev: 0x0, Func: 0}, writeEmpty: true, additionalComment: "Host bridge"},
					PCISlot{PCIAddr: PCIAddr{Dev: 0x1, Func: 0}, writeEmpty: true, additionalComment: "PCIe Bridge for discrete graphics"},
					PCISlot{PCIAddr: PCIAddr{Dev: 0x2, Func: 0}, writeEmpty: true, additionalComment: "Internal graphics"},
				},
			},
		},
	}

	PutPCIDev(addr, "Host bridge")

	/* FIXME:XX some configs are unsupported.  */

	KconfigBool["CPU_INTEL_HASWELL"] = true
	KconfigBool["NORTHBRIDGE_INTEL_HASWELL"] = true
	KconfigBool["INTEL_INT15"] = true
	KconfigBool["HAVE_ACPI_TABLES"] = true
	KconfigBool["HAVE_ACPI_RESUME"] = true

	KconfigInt["MAX_CPUS"] = 8

	DSDTIncludes = append(DSDTIncludes, DSDTInclude{
		File: "cpu/intel/common/acpi/cpu.asl",
	})

	DSDTPCI0Includes = append(DSDTPCI0Includes, DSDTInclude{
		File: "northbridge/intel/haswell/acpi/haswell.asl",
	}, DSDTInclude{
		File: "drivers/intel/gma/acpi/default_brightness_levels.asl",
	})
}

func init() {
	RegisterPCI(0x8086, 0x0c00, haswellmc{variant: "Desktop"})
	RegisterPCI(0x8086, 0x0c04, haswellmc{variant: "Mobile"})
	RegisterPCI(0x8086, 0x0a04, haswellmc{variant: "ULT"})
	RegisterPCI(0x8086, 0x0c08, haswellmc{variant: "Server"})
	for _, id := range []uint16{
		0x0402, 0x0412, 0x0422,
		0x0406, 0x0416, 0x0426,
		0x0d16, 0x0d26, 0x0d36,
		0x0a06, 0x0a16, 0x0a26,
	} {
		RegisterPCI(0x8086, id, GenericVGA{GenericPCI{Comment: "VGA controller"}})
	}
}
