/* extension.js
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

import GObject from 'gi://GObject';
import St from 'gi://St';
import Clutter from 'gi://Clutter';
import NM from 'gi://NM';
import GLib from 'gi://GLib';

import {Extension, gettext as _} from 'resource:///org/gnome/shell/extensions/extension.js';
import * as PanelMenu from 'resource:///org/gnome/shell/ui/panelMenu.js';
import * as PopupMenu from 'resource:///org/gnome/shell/ui/popupMenu.js';

import * as Main from 'resource:///org/gnome/shell/ui/main.js';

const Indicator = GObject.registerClass(
class Indicator extends PanelMenu.Button {
    _init() {
        // TODO: why is this and how to change it
        super._init(0.0, _('My Shiny Indicator'));

        this._box = new St.BoxLayout({ vertical: false });
        this._icon = new St.Icon({ icon_name: 'face-smile-symbolic', style_class: 'system-status-icon', style: 'color: #7a7a7a' });
        this._label = new St.Label({ text: 'None', y_align: Clutter.ActorAlign.CENTER });

        this._box.add_child(this._icon);
        this._box.add_child(this._label);
        this.add_child(this._box);

        this._nmClient = NM.Client.new(null);

        this._timeoutID = GLib.timeout_add_seconds(
            GLib.PRIORITY_DEFAULT,
            5,
            () => {
                let state = this._getConnName();

                this._label.text = state.label;
                this._icon.set_icon_name(state.icon);
                this._icon.set_style(`color: ${state.color};`);

                return GLib.SOURCE_CONTINUE;
            }
        );

        // TODO: make this drop-down useful
        // -> settings: set the timeout
        // -> make `prod` to blink and add settings to disable this
        let item = new PopupMenu.PopupMenuItem(_('Show Notification'));
        item.connect('activate', () => {
            Main.notify(_('Active conns: ' + this._getConnName()));
        });

        this.menu.addMenuItem(item);
    }

    _getConnName() {
        // TODO: extract this
        const map = {
            none: {
                icon: 'face-smile-symbolic',
                color: '#7a7a7a',
                label: 'None',
            },
            dev: {
                icon: 'face-plain-symbolic',
                color: '#ffffff',
                label: 'Dev',
            },
            demo: {
                icon: 'face-worried-symbolic',
                color: '#f5c542',
                label: 'Demo',
            },
            prod: {
                icon: 'face-angry-symbolic',
                color: '#e64545',
                label: 'Prod',
            }
        };

        const activeConns = this._nmClient.get_active_connections() || 'None';

        let vpnName = 'none';

        for (const conn of activeConns) {
            if (conn.get_connection_type() == 'vpn') {
                vpnName = conn.get_id();
            }
        }

        return map[vpnName];
    }

    destroy() {
        if (this._timeoutID) {
            GLib.source_remove(this._timeoutID);
            this._timeoutID = 0;
        }

        super.destroy();
    }
});

export default class IndicatorExampleExtension extends Extension {
    enable() {
        this._indicator = new Indicator();
        Main.panel.addToStatusArea(this.uuid, this._indicator);
    }

    disable() {
        this._indicator.destroy();
        this._indicator = null;
    }
}
