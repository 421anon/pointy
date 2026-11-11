import { Type } from "@earendil-works/pi-ai";
import { defineTool, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

type ContentPart = { type: string; text?: string; data?: string; mimeType?: string };

const showImageTool = defineTool({
	name: "show_image",
	label: "Show image",
	description:
		"Show an image file to the user. It appears in the chat at this point of your reply. The user sees no image you only read or write, so call this for every image you want them to look at. Accepts PNG, JPEG, GIF, and WebP.",
	parameters: Type.Object({
		path: Type.String({ description: "Path to the image file, absolute or relative to the working directory" }),
	}),

	async execute(_toolCallId, params, signal, _onUpdate, ctx) {
		const { result, isError } = await ctx.executeTool("read", { path: params.path }, { signal });
		const content = result.content as ContentPart[];
		if (isError) {
			throw new Error(
				content
					.filter((part) => part.type === "text")
					.map((part) => part.text)
					.join("\n"),
			);
		}
		const image = content.find((part) => part.type === "image");
		if (!image) throw new Error(`${params.path} is not a PNG, JPEG, GIF, or WebP image`);
		return {
			content: [{ type: "text", text: `The user now sees ${params.path} in the chat.` }],
			details: { mimeType: image.mimeType, data: image.data },
		};
	},
});

export default function (pi: ExtensionAPI) {
	pi.registerTool(showImageTool);
}
